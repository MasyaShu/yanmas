package ru.itterminal.botdesk.integration.aws.ses.flow;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.integration.aws.AwsConfig;
import ru.itterminal.botdesk.integration.aws.ses.AwsSesConfig;
import ru.itterminal.botdesk.integration.aws.ses.SenderEmailViaAwsSes;
import ru.itterminal.botdesk.integration.config.IntegrationConfig;
import software.amazon.awssdk.services.ses.model.SendRawEmailRequest;

@Slf4j
@SpringBootTest(classes = {IntegrationConfig.class, AwsConfig.class, AwsSesConfig.class,
        SenderEmailViaAwsSes.class, SendingEmailViaAwsSesFlow.class})
class SendingEmailViaAwsSesFlowTest {

    @SuppressWarnings("SpringJavaInjectionPointsAutowiringInspection")
    @Autowired
    private SendingEmailViaAwsSesFlow.MailSenderViaAwsSesMessagingGateway gateway;

    private static final String SOME_MESSAGE_ID = "fgnsdjgsjgn43454n3r34b5235354";

    @MockBean
    SenderEmailViaAwsSes senderEmailViaAwsSes;

    @Test
    void SenderEmailViaAwsSesIntegrationFlow_shouldGetMessageID_whenPassedValidRawMessage() {
        when(senderEmailViaAwsSes.sendEmail(any())).thenReturn(SOME_MESSAGE_ID);
        SendRawEmailRequest rawEmailRequest = SendRawEmailRequest.builder().build();
        String messageId = gateway.process(rawEmailRequest);
        assertEquals(SOME_MESSAGE_ID, messageId);
    }

}