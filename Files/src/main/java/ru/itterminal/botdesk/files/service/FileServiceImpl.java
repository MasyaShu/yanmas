package ru.itterminal.botdesk.files.service;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createLogicalValidationException;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.FileSystemResource;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.files.model.File;
import ru.itterminal.botdesk.files.repository.FileRepository;
import ru.itterminal.botdesk.files.repository.FileSystemRepository;
import ru.itterminal.botdesk.files.service.validator.FileOperationValidator;

@Slf4j
@Service
public class FileServiceImpl extends CrudServiceWithAccountImpl<File, FileOperationValidator, FileRepository> {

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
    private final AccountServiceImpl accountService;

    public FileServiceImpl(FileSystemRepository fileSystemRepository,
                           AccountServiceImpl accountService,
                           @Value("${dir.uploaded.files}") String dirUploadedFiles) throws IOException {
        this.fileSystemRepository = fileSystemRepository;
        this.accountService = accountService;
        this.dirUploadedFiles = dirUploadedFiles;
        Files.createDirectories(Paths.get(dirUploadedFiles));
    }

    @Transactional(readOnly = true)
    public FileSystemResource getFileData(UUID accountId, UUID fileId) {
        if (fileId == null) {
            throw createLogicalValidationException(FILE_ID, FILE_ID_IS_NULL);
        }
        var file = super.findByIdAndAccountId(fileId);
        if (Boolean.FALSE.equals(file.getIsUploaded())) {
            throw createLogicalValidationException(FILE, FILE_WAS_NOT_UPLOAD);
        }
        return fileSystemRepository.findInFileSystem(
                Paths.get(
                        dirUploadedFiles,
                        accountId.toString(),
                        fileId.toString()
                )
        );
    }

    @Transactional
    public void putFileData(UUID accountId, UUID authorId, UUID fileId, byte[] bytes) throws IOException {
        if (fileId == null) {
            throw createLogicalValidationException(FILE_ID, FILE_ID_IS_NULL);
        }
        if (bytes.length > maxSizeOfFile) {
            throw createLogicalValidationException(SIZE_FILE, format(MAX_SIZE, maxSizeOfFile));
        }
        File file = repository.findByAccountIdAndAuthorIdAndId(accountId, authorId, fileId).orElseThrow(
                () -> {
                    var errorMessage = format(COULD_NOT_FIND_FILE, SEARCH_PARAMETER, accountId, authorId, fileId);
                    log.error(errorMessage);
                    throw new EntityNotExistException(errorMessage);
                }
        );
        fileSystemRepository.save(
                bytes,
                Paths.get(
                        dirUploadedFiles,
                        accountId.toString(),
                        fileId.toString()
                )
        );
        file.setIsUploaded(true);
        file.setSize(bytes.length);
        repository.update(file);
    }

    @Override
    protected void setNestedObjectsOfEntityBeforeCreate(File entity) {
        entity.setAccount(accountService.findById(entity.getAccount().getId()));
    }

    @Override
    protected void setNestedObjectsOfEntityBeforeUpdate(File entity) {
        entity.setAccount(accountService.findById(entity.getAccount().getId()));
    }
}
