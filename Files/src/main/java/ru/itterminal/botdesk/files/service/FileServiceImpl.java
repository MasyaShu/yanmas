package ru.itterminal.botdesk.files.service;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekObjectForNull;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.ifErrorsNotEmptyThrowLogicalValidationException;

import java.nio.ByteBuffer;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.extern.slf4j.Slf4j;
import lombok.val;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceImpl;
import ru.itterminal.botdesk.files.model.File;
import ru.itterminal.botdesk.files.repository.FileRepository;
import ru.itterminal.botdesk.files.service.validator.FileOperationValidator;
import ru.itterminal.botdesk.integration.aws.s3.AwsS3ObjectOperations;

@Slf4j
@Service
@Transactional
public class FileServiceImpl extends CrudServiceImpl<File, FileOperationValidator, FileRepository> {

    private static final String METHOD_UPDATE = "For now this method doesn't implement yet";
    private static final String BYTES_OF_FILE = "Bytes of file";
    private static final String BYTES_OF_FILE_IS_NULL = "Bytes of file is null";
    private static final String ACCOUNT_ID = "Account id";
    private static final String ACCOUNT_ID_IS_NULL = "Account id is null";
    private static final String FILE_ID = "File id";
    private static final String FILE_ID_IS_NULL = "File id is null";
    private static final String NOT_FOUND_FILE = "Not found file by accountId: %s and fileId: %s";

    private final AwsS3ObjectOperations awsS3ObjectOperations;

    @Autowired
    public FileServiceImpl(AwsS3ObjectOperations awsS3ObjectOperations) {
        this.awsS3ObjectOperations = awsS3ObjectOperations;
    }

    @Override
    public File update(File entity) {
        throw new UnsupportedOperationException(METHOD_UPDATE);
    }

    @Transactional(readOnly = true)
    public byte[] getFileData(UUID accountId, UUID fileId) {
        val logicalErrors = createMapForLogicalErrors();
        chekObjectForNull(accountId, ACCOUNT_ID, ACCOUNT_ID_IS_NULL, logicalErrors);
        chekObjectForNull(fileId, FILE_ID, FILE_ID_IS_NULL, logicalErrors);
        ifErrorsNotEmptyThrowLogicalValidationException(logicalErrors);
        // TODO if getIsUploaded()==true
        return awsS3ObjectOperations.getObject(accountId.toString(), fileId.toString());
    }

    public boolean putFileData(UUID accountId, UUID fileId, byte[] bytes) {
        val logicalErrors = createMapForLogicalErrors();
        chekObjectForNull(bytes, BYTES_OF_FILE, BYTES_OF_FILE_IS_NULL, logicalErrors);
        chekObjectForNull(accountId, ACCOUNT_ID, ACCOUNT_ID_IS_NULL, logicalErrors);
        chekObjectForNull(fileId, FILE_ID, FILE_ID_IS_NULL, logicalErrors);
        ifErrorsNotEmptyThrowLogicalValidationException(logicalErrors);
        File fileFromDatabase = repository.findByAccountIdAndId(accountId, fileId).orElseThrow(() -> {
            String messageException = format(NOT_FOUND_FILE, accountId, fileId);
            log.trace(messageException);
            throw new EntityNotExistException(messageException);
        });
        Boolean result = awsS3ObjectOperations.putObject(accountId.toString(), fileId.toString(), ByteBuffer.wrap(bytes));
        if (!fileFromDatabase.getIsUploaded().equals(result)) {
            fileFromDatabase.setIsUploaded(result);
            repository.save(fileFromDatabase);
        }
        return result;
    }
}
