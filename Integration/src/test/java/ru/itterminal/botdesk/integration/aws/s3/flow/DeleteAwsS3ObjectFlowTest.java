package ru.itterminal.botdesk.integration.aws.s3.flow;

import static org.awaitility.Awaitility.await;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

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
        S3ObjectOperations.class, DeleteAwsS3ObjectFlow.class})
class DeleteAwsS3ObjectFlowTest {

    @SuppressWarnings("SpringJavaInjectionPointsAutowiringInspection")
    @Autowired
    private DeleteAwsS3ObjectFlow.DeleteAwsS3ObjectGateway gateway;

    @MockBean
    S3ObjectOperations s3ObjectOperations;

    @Test
    void putAwsS3ObjectFlow_shouldPutAwsObjectInBucket_whenPassedValidData() {
        AwsS3Object awsS3Object = AwsS3Object.builder()
                .bucketName("BUCKET_NAME")
                .objectName("OBJECT_NAME")
                .build();
        gateway.process(awsS3Object);
        await().pollDelay(Durations.ONE_SECOND).until(() -> true);
        verify(s3ObjectOperations, times(1))
                .deleteObject(awsS3Object.getBucketName(), awsS3Object.getObjectName());
    }

}