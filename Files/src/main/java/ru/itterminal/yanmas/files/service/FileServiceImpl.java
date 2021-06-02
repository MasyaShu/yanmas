package ru.itterminal.yanmas.files.service;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createLogicalValidationException;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.FileSystemResource;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.CrudServiceWithBusinessHandlerImpl;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.EntityNotExistException;
import ru.itterminal.yanmas.files.model.File;
import ru.itterminal.yanmas.files.repository.FileRepository;
import ru.itterminal.yanmas.files.repository.FileSystemRepository;

@Service
public class FileServiceImpl extends CrudServiceWithBusinessHandlerImpl
        <File, FileRepository> {

    public static final String FILE = "File";
    public static final String FILE_WAS_NOT_UPLOAD = "File wasn't upload";
    public static final String FILE_WAS_ALREADY_UPLOADED = "File was already uploaded";
    public static final String SIZE_FILE = "Size of file";
    public static final String MAX_SIZE = "Size of file mustn't over %s bytes";
    public static final String COULD_NOT_FIND_FILE = "Could not find entity by %s: '%s', '%s', '%s'";
    public static final String SEARCH_PARAMETER = "accountId, authorId and fileId";

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
        var file = repository
                .findByAccountIdAndAuthorIdAndId(currentUser.getAccount().getId(), currentUser.getId(), fileId)
                .orElseThrow(
                        () -> {
                            var errorMessage =
                                    format(COULD_NOT_FIND_FILE, SEARCH_PARAMETER, currentUser.getAccount().getId(),
                                           currentUser.getId(), fileId
                                    );
                            throw new EntityNotExistException(errorMessage);
                        }
                );
        if (Boolean.TRUE.equals(file.getIsUploaded())) {
            throw createLogicalValidationException(FILE, FILE_WAS_ALREADY_UPLOADED);
        }
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
