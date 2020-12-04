package ru.itterminal.botdesk.files.service;

import java.nio.ByteBuffer;
import java.util.List;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.extern.slf4j.Slf4j;
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

    private static final String OTHER_METHOD_CREATE = "For create file must use method create(File entity, byte[] bytes)";
    private static final String METHOD_UPDATE = "For now this method doesn't implement yet";

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
        entity.setSize(bytes.length);
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
    public List<File> findAllByEntityIdAndAccountId(UUID entityId, UUID accountId) {
        return repository.findAllByEntityIdAndAccount_Id(entityId, accountId);
    }

    public byte[] getFileData(String accountId, String fileId) {
        return awsS3ObjectOperations.getObject(accountId, fileId);
    }
}
