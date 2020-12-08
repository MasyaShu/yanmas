package ru.itterminal.botdesk.files.service;

import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekObjectForNull;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.throwLogicalValidationExceptionIfErrorsNotEmpty;

import java.nio.ByteBuffer;
import java.util.List;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.extern.slf4j.Slf4j;
import lombok.val;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceImpl;
import ru.itterminal.botdesk.files.model.File;
import ru.itterminal.botdesk.files.repository.FileRepository;
import ru.itterminal.botdesk.files.service.validator.FileOperationValidator;
import ru.itterminal.botdesk.integration.aws.s3.AwsS3Object;
import ru.itterminal.botdesk.integration.aws.s3.AwsS3ObjectOperations;
import ru.itterminal.botdesk.integration.aws.s3.flow.PutAwsS3ObjectFlow;

@Slf4j
@Service
@Transactional
public class FileServiceImpl extends CrudServiceImpl<File, FileOperationValidator, FileRepository> {

    private static final String OTHER_METHOD_CREATE =
            "For create file must use method create(File entity, byte[] bytes)";
    private static final String METHOD_UPDATE = "For now this method doesn't implement yet";
    private static final String BYTES_OF_FILE = "Bytes of file";
    private static final String BYTES_OF_FILE_IS_NULL = "Bytes of file is null";
    private static final String ACCOUNT_ID = "Account id";
    private static final String ACCOUNT_ID_IS_NULL = "Account id is null";
    private static final String ENTITY_ID = "Entity id";
    private static final String ENTITY_ID_IS_NULL = "Entity id is null";
    private static final String FILE_ID = "File id";
    private static final String FILE_ID_IS_NULL = "File id is null";

    private final PutAwsS3ObjectFlow.PutAwsS3ObjectGateway putAwsS3ObjectGateway;
    private final AwsS3ObjectOperations awsS3ObjectOperations;

    @SuppressWarnings("SpringJavaInjectionPointsAutowiringInspection")
    @Autowired
    public FileServiceImpl(PutAwsS3ObjectFlow.PutAwsS3ObjectGateway putAwsS3ObjectGateway,
                           AwsS3ObjectOperations awsS3ObjectOperations) {
        this.putAwsS3ObjectGateway = putAwsS3ObjectGateway;
        this.awsS3ObjectOperations = awsS3ObjectOperations;
    }

    @Override
    public File create(File entity) {
        throw new UnsupportedOperationException(OTHER_METHOD_CREATE);
    }

    @Override
    public File update(File entity) {
        throw new UnsupportedOperationException(METHOD_UPDATE);
    }

    public File create(File entity, byte[] bytes) {
        val logicalErrors = createMapForLogicalErrors();
        logicalErrors.putAll(chekObjectForNull(bytes, BYTES_OF_FILE, BYTES_OF_FILE_IS_NULL));
        throwLogicalValidationExceptionIfErrorsNotEmpty(logicalErrors);
        File createdFile = super.create(entity);
        AwsS3Object awsS3Object = AwsS3Object.builder()
                .bucketName(createdFile.getAccount().getId().toString())
                .objectName(createdFile.getId().toString())
                .byteBuffer(ByteBuffer.wrap(bytes))
                .build();
        putAwsS3ObjectGateway.process(awsS3Object);
        return createdFile;
    }

    @Transactional(readOnly = true)
    public List<File> findAllByEntityIdAndAccountId(UUID accountId, UUID entityId) {
        val logicalErrors = createMapForLogicalErrors();
        logicalErrors.putAll(chekObjectForNull(accountId, ACCOUNT_ID, ACCOUNT_ID_IS_NULL));
        logicalErrors.putAll(chekObjectForNull(entityId, ENTITY_ID, ENTITY_ID_IS_NULL));
        throwLogicalValidationExceptionIfErrorsNotEmpty(logicalErrors);
        return repository.findAllByEntityIdAndAccount_Id(entityId, accountId);
    }

    @Transactional(readOnly = true)
    public byte[] getFileData(UUID accountId, UUID fileId) {
        val logicalErrors = createMapForLogicalErrors();
        logicalErrors.putAll(chekObjectForNull(accountId, ACCOUNT_ID, ACCOUNT_ID_IS_NULL));
        logicalErrors.putAll(chekObjectForNull(fileId, FILE_ID, FILE_ID_IS_NULL));
        throwLogicalValidationExceptionIfErrorsNotEmpty(logicalErrors);
        return awsS3ObjectOperations.getObject(accountId.toString(), fileId.toString());
    }
}
