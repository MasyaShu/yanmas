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
import ru.itterminal.botdesk.integration.aws.s3.AwsS3BucketOperations;
import ru.itterminal.botdesk.integration.config.IntegrationConfig;

@SpringJUnitConfig(value = {IntegrationConfig.class, AwsConfig.class, AwsS3Config.class,
        AwsS3BucketOperations.class, DeleteAwsS3BucketFlow.class})
class DeleteAwsS3BucketFlowTest {

    @SuppressWarnings("SpringJavaInjectionPointsAutowiringInspection")
    @Autowired
    private DeleteAwsS3BucketFlow.DeleteAwsBucketGateway gateway;

    @MockBean
    AwsS3BucketOperations awsS3BucketOperations;

    @Test
    void deleteAwsBucketFlow_shouldDeleteAwsBucket_whenPassedUniqueBucketName() {
        String NAME_BUCKET = "Test bucket name";
        gateway.process(NAME_BUCKET);
        await().pollDelay(Durations.ONE_SECOND).until(() -> true);
        verify(awsS3BucketOperations, times(1)).deleteBucket(NAME_BUCKET);
    }

}