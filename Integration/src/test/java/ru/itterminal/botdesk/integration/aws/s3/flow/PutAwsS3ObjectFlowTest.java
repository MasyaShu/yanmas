package ru.itterminal.botdesk.integration.aws.s3.flow;

import static org.awaitility.Awaitility.await;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.nio.ByteBuffer;
import java.util.Random;

import org.awaitility.Durations;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.integration.aws.AwsConfig;
import ru.itterminal.botdesk.integration.aws.s3.AwsS3Config;
import ru.itterminal.botdesk.integration.aws.s3.AwsS3Object;
import ru.itterminal.botdesk.integration.aws.s3.S3ObjectOperations;
import ru.itterminal.botdesk.integration.config.IntegrationConfig;

@SpringJUnitConfig(value = {IntegrationConfig.class, AwsConfig.class, AwsS3Config.class,
        S3ObjectOperations.class, PutAwsS3ObjectFlow.class})
class PutAwsS3ObjectFlowTest {

    @SuppressWarnings("SpringJavaInjectionPointsAutowiringInspection")
    @Autowired
    private PutAwsS3ObjectFlow.PutAwsS3ObjectGateway gateway;

    @MockBean
    S3ObjectOperations s3ObjectOperations;

    @Test
    void putAwsS3ObjectFlow_shouldPutAwsObjectInBucket_whenPassedValidData() {
        byte[] data = new byte[10];
        new Random().nextBytes(data);
        AwsS3Object awsS3Object = AwsS3Object.builder()
                .bucketName("BUCKET_NAME")
                .objectName("OBJECT_NAME")
                .byteBuffer(ByteBuffer.wrap(data))
                .build();
        gateway.process(awsS3Object);
        await().pollDelay(Durations.ONE_SECOND).until(() -> true);
        verify(s3ObjectOperations, times(1))
                .putObject(awsS3Object.getBucketName(), awsS3Object.getObjectName(), awsS3Object.getByteBuffer());
    }

}