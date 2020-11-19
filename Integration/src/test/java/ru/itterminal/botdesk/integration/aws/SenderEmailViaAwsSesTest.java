package ru.itterminal.botdesk.integration.aws;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static ru.itterminal.botdesk.integration.aws.SenderEmailViaAwsSes.createEmail;

import javax.annotation.PostConstruct;

import org.joda.time.DateTime;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.TestPropertySource;

import com.amazonaws.services.simpleemail.AmazonSimpleEmailService;
import com.amazonaws.services.simpleemail.model.SendEmailRequest;
import com.amazonaws.services.simpleemail.model.SendEmailResult;

import ru.itterminal.botdesk.integration.config.GeneralConfig;

@SpringBootTest(classes = GeneralConfig.class)
@TestPropertySource(properties = {"aws.accessKeyId=AKIAUUSU2LYV5JFH7WOV", "aws.secretKey=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",
        "aws.ses.region=eu-central-1", "aws.ses.email.noReplay=noreply@botdesk.app"})
class SenderEmailViaAwsSesTest {

    @SuppressWarnings("SpringJavaInjectionPointsAutowiringInspection")
    @Autowired
    private SenderEmailViaAwsSes.MailSenderViaAwsSesMessagingGateway gateway;

    private static final String RECIPIENT = "unit-test@botdesk.app";
    private static final String SOME_MESSAGE_ID = "fgnsdjgsjgn43454n3r34b5235354";

    @Value("${aws.accessKeyId}")
    private String accessKeyId;

    @Value("${aws.secretKey}")
    private String secretKey;

    @PostConstruct
    void setSystemProperty(){
        System.setProperty("aws.accessKeyId", accessKeyId);
        System.setProperty("aws.secretKey", secretKey);
    }

    @MockBean
    AmazonSimpleEmailService amazonSimpleEmailService;

    @Mock
    SendEmailResult sendEmailResult;


    @Test
    void SenderEmailViaAwsSesIntegrationFlow_shouldSendEmail_whenPassedValidSendEmailRequest() {
        when(amazonSimpleEmailService.sendEmail(any())).thenReturn(sendEmailResult);
        when(sendEmailResult.getMessageId()).thenReturn(SOME_MESSAGE_ID);
        SendEmailRequest email = createEmail(RECIPIENT,
                "Test message from UnitTest of BotDesk : " + new DateTime().toString(),
                "BotDesk. Тест сообщения");
        String messageId = gateway.process(email);
        assertEquals(SOME_MESSAGE_ID, messageId);
    }

}