package ru.itterminal.botdesk.files.service;

import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekObjectForNull;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.ifErrorsNotEmptyThrowLogicalValidationException;

import java.nio.ByteBuffer;
import java.util.UUID;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import lombok.val;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.files.model.File;
import ru.itterminal.botdesk.files.repository.FileRepository;
import ru.itterminal.botdesk.files.service.validator.FileOperationValidator;
import ru.itterminal.botdesk.integration.aws.s3.AwsS3ObjectOperations;

@Slf4j
@Service
@Transactional
@RequiredArgsConstructor
public class FileServiceImpl extends CrudServiceWithAccountImpl<File, FileOperationValidator, FileRepository> {

    private static final String METHOD_UPDATE = "For now this method doesn't implement yet";
    private static final String FILE_ID = "File id";
    private static final String FILE_ID_IS_NULL = "File id is null";
    public static final String FILE = "File";
    public static final String FILE_WAS_NOT_UPLOAD = "File wasn't upload";

    private final AwsS3ObjectOperations awsS3ObjectOperations;

    @Override
    public File update(File entity) {
        throw new UnsupportedOperationException(METHOD_UPDATE);
    }

    @Transactional(readOnly = true)
    public byte[] getFileData(UUID accountId, UUID fileId) {
        File file =  super.findByIdAndAccountId(fileId, accountId);
        val logicalErrors = createMapForLogicalErrors();
        if (file.getIsUploaded().equals(false)) {
            addValidationErrorIntoErrors(FILE, FILE_WAS_NOT_UPLOAD, logicalErrors);
        }
        ifErrorsNotEmptyThrowLogicalValidationException(logicalErrors);
        return awsS3ObjectOperations.getObject(accountId.toString(), fileId.toString());
    }

    public boolean putFileData(UUID accountId, UUID fileId, byte[] bytes) {
        val logicalErrors = createMapForLogicalErrors();
        chekObjectForNull(fileId, FILE_ID, FILE_ID_IS_NULL, logicalErrors);
        ifErrorsNotEmptyThrowLogicalValidationException(logicalErrors);
        File file = super.findByIdAndAccountId(fileId, accountId);
        boolean isPutFileData = awsS3ObjectOperations.putObject(accountId.toString(), fileId.toString(), ByteBuffer.wrap(bytes));
        if (isPutFileData) {
            file.setIsUploaded(true);
            repository.save(file);
        }
        return isPutFileData;
    }
}
