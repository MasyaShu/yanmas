package ru.itterminal.botdesk.integration.aws.s3.flow;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.integration.aws.AwsConfig;
import ru.itterminal.botdesk.integration.aws.s3.AwsS3Config;
import ru.itterminal.botdesk.integration.aws.s3.S3BucketOperations;
import ru.itterminal.botdesk.integration.aws.s3.S3ObjectOperations;
import ru.itterminal.botdesk.integration.aws.ses.AwsSesConfig;
import ru.itterminal.botdesk.integration.aws.ses.SenderEmailViaAwsSes;
import ru.itterminal.botdesk.integration.aws.ses.flow.SendingEmailViaAwsSesFlow;
import ru.itterminal.botdesk.integration.config.IntegrationConfig;
import software.amazon.awssdk.services.ses.model.SendRawEmailRequest;

@Slf4j
@SpringBootTest(classes = {IntegrationConfig.class, AwsConfig.class, AwsS3Config.class,
        S3BucketOperations.class, CreateAwsBucketFlow.class})
class CreateAwsBucketFlowTest {

    @SuppressWarnings("SpringJavaInjectionPointsAutowiringInspection")
    @Autowired
    private CreateAwsBucketFlow.CreateAwsBucketGateway gateway;

    private final String NAME_BUCKET = "Test bucket name";

    @MockBean
    S3BucketOperations s3BucketOperations;

    @Test
    void createAwsBucketFlow_shouldCreateAwsBucket_whenPassedUniqueBucketName() {
        gateway.process(NAME_BUCKET);
        verify(s3BucketOperations, times(1)).createBucket(NAME_BUCKET);
    }

}