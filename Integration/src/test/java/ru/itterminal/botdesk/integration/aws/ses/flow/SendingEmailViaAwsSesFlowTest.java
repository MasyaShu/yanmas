package ru.itterminal.botdesk.integration.aws.ses.flow;

import static org.awaitility.Awaitility.await;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.awaitility.Durations;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.integration.aws.AwsConfig;
import ru.itterminal.botdesk.integration.aws.ses.AwsSesConfig;
import ru.itterminal.botdesk.integration.aws.ses.SenderEmailViaAwsSes;
import ru.itterminal.botdesk.integration.config.IntegrationConfig;
import software.amazon.awssdk.services.ses.model.SendRawEmailRequest;

@SpringJUnitConfig(value = {IntegrationConfig.class, AwsConfig.class, AwsSesConfig.class,
        SenderEmailViaAwsSes.class, SendingEmailViaAwsSesFlow.class})
class SendingEmailViaAwsSesFlowTest {

    @SuppressWarnings("SpringJavaInjectionPointsAutowiringInspection")
    @Autowired
    private SendingEmailViaAwsSesFlow.MailSenderViaAwsSesMessagingGateway gateway;

    @MockBean
    SenderEmailViaAwsSes senderEmailViaAwsSes;

    @Test
    void SenderEmailViaAwsSesIntegrationFlow_shouldCallSendEmailWithAccordingParameter_whenPassedValidRawMessage() {
        SendRawEmailRequest rawEmailRequest = SendRawEmailRequest.builder().build();
        gateway.process(rawEmailRequest);
        await().pollDelay(Durations.ONE_SECOND).until(() -> true);
        verify(senderEmailViaAwsSes, times(1)).sendEmail(rawEmailRequest);
    }

}