package ru.itterminal.botdesk.files.service;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createExpectedLogicalValidationException;

import java.nio.ByteBuffer;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.files.model.File;
import ru.itterminal.botdesk.files.repository.FileRepository;
import ru.itterminal.botdesk.files.service.validator.FileOperationValidator;
import ru.itterminal.botdesk.integration.aws.s3.AwsS3ObjectOperations;

@Slf4j
@Service
@RequiredArgsConstructor
public class FileServiceImpl extends CrudServiceWithAccountImpl<File, FileOperationValidator, FileRepository> {

    public static final String FILE_ID = "File id";
    public static final String FILE_ID_IS_NULL = "File id is null";
    public static final String FILE = "File";
    public static final String FILE_WAS_NOT_UPLOAD = "File wasn't upload";
    public static final String SIZE_FILE = "Size of file";
    public static final String MAX_SIZE = "Size of file mustn't over %s bytes";

    @Value("${maxSizeOfFile}")
    private Long maxSizeOfFile;
    private final AwsS3ObjectOperations awsS3ObjectOperations;

    @Transactional(readOnly = true)
    public byte[] getFileData(UUID accountId, UUID fileId) {
        if (fileId == null) {
            throw createExpectedLogicalValidationException(FILE_ID, FILE_ID_IS_NULL);
        }
        var file = super.findByIdAndAccountId(fileId, accountId);
        if (Boolean.FALSE.equals(file.getIsUploaded())) {
            throw createExpectedLogicalValidationException(FILE, FILE_WAS_NOT_UPLOAD);
        }
        return awsS3ObjectOperations.getObject(accountId, fileId);
    }

    @Transactional
    public boolean putFileData(UUID accountId, UUID fileId, byte[] bytes) {
        if (fileId == null) {
            throw createExpectedLogicalValidationException(FILE_ID, FILE_ID_IS_NULL);
        }
        if (bytes.length > maxSizeOfFile) {
            throw createExpectedLogicalValidationException(SIZE_FILE, format(MAX_SIZE, maxSizeOfFile));
        }
        File file = super.findByIdAndAccountId(fileId, accountId);
        var isFileUploaded = awsS3ObjectOperations.putObject(accountId, fileId, ByteBuffer.wrap(bytes));
        if (isFileUploaded) {
            file.setIsUploaded(true);
            file.setSize(bytes.length);
            repository.update(file);
        }
        return isFileUploaded;
    }
}
