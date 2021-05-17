package ru.itterminal.yanmas.files.service;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.FileSystemResource;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.impl.CrudServiceWithBusinessHandlerImpl;
import ru.itterminal.yanmas.aau.service.business_handler.impl.EmptyBusinessHandlerImpl;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.EntityNotExistException;
import ru.itterminal.yanmas.files.model.File;
import ru.itterminal.yanmas.files.repository.FileRepository;
import ru.itterminal.yanmas.files.repository.FileSystemRepository;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.UUID;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createLogicalValidationException;

@Slf4j
@Service
public class FileServiceImpl extends CrudServiceWithBusinessHandlerImpl
        <File, EmptyBusinessHandlerImpl<File>, FileRepository> {

    public static final String FILE_ID = "File id";
    public static final String FILE_ID_IS_NULL = "File id is null";
    public static final String FILE = "File";
    public static final String FILE_WAS_NOT_UPLOAD = "File wasn't upload";
    public static final String SIZE_FILE = "Size of file";
    public static final String MAX_SIZE = "Size of file mustn't over %s bytes";
    public static final String COULD_NOT_FIND_FILE = "Could not find entity by %s: '%s', '%s', '%s'";
    public static final String SEARCH_PARAMETER = "accountId, authorId and fileId";

    @Value("${maxSizeOfFile}")
    private Long maxSizeOfFile;
    private final String dirUploadedFiles;

    private final FileSystemRepository fileSystemRepository;

    public FileServiceImpl(FileSystemRepository fileSystemRepository,
                           @Value("${dir.uploaded.files}") String dirUploadedFiles,
                           List<EntityValidator<File>> validators) throws IOException {
        this.fileSystemRepository = fileSystemRepository;
        this.dirUploadedFiles = dirUploadedFiles;
        this.validators = validators;
        Files.createDirectories(Paths.get(dirUploadedFiles));
    }

    @Transactional(readOnly = true)
    public FileSystemResource getFileData(User currentUser, UUID fileId) {
        if (fileId == null) {
            throw createLogicalValidationException(FILE_ID, FILE_ID_IS_NULL);
        }
        var file = findByIdAndAccountId(fileId, currentUser);
        if (Boolean.FALSE.equals(file.getIsUploaded())) {
            throw createLogicalValidationException(FILE, FILE_WAS_NOT_UPLOAD);
        }
        return fileSystemRepository.findInFileSystem(
                Paths.get(
                        dirUploadedFiles,
                        currentUser.getAccount().getId().toString(),
                        fileId.toString()
                )
        );
    }

    @Transactional
    public void putFileData(User currentUser, UUID fileId, byte[] bytes) throws IOException {
        if (fileId == null) {
            throw createLogicalValidationException(FILE_ID, FILE_ID_IS_NULL);
        }
        if (bytes.length > maxSizeOfFile) {
            throw createLogicalValidationException(SIZE_FILE, format(MAX_SIZE, maxSizeOfFile));
        }
        var file = repository.findByAccountIdAndAuthorIdAndId(currentUser.getAccount().getId(), currentUser.getId(), fileId).orElseThrow(
                () -> {
                    var errorMessage = format(COULD_NOT_FIND_FILE, SEARCH_PARAMETER, currentUser.getAccount().getId(), currentUser.getId(), fileId);
                    log.error(errorMessage);
                    throw new EntityNotExistException(errorMessage);
                }
        );
        fileSystemRepository.save(
                bytes,
                Paths.get(
                        dirUploadedFiles,
                        currentUser.getAccount().getId().toString(),
                        fileId.toString()
                )
        );
        file.setIsUploaded(true);
        file.setSize(bytes.length);
        repository.update(file);
    }
}
