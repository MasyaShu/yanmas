package ru.itterminal.botdesk.integration.aws;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.joda.time.DateTime;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import com.amazonaws.services.simpleemail.model.Body;
import com.amazonaws.services.simpleemail.model.Content;
import com.amazonaws.services.simpleemail.model.Destination;
import com.amazonaws.services.simpleemail.model.Message;
import com.amazonaws.services.simpleemail.model.SendEmailRequest;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {SenderEmailViaAwsSes.class})
@TestPropertySource(properties = {"aws.ses.email.noReplay=noreply@botdesk.app", "aws.ses.region=eu-central-1"})
class SenderEmailViaAwsSesTest {

    @Autowired
    SenderEmailViaAwsSes amazonSES;

    private static SendEmailRequest sendEmailRequest;
    private static final String RECIPIENT = "unit-test@botdesk.app";

    @BeforeAll
    void setUpBeforeAll() {
        System.setProperty("aws.accessKeyId", "AKIAUUSU2LYV5JFH7WOV");
        System.setProperty("aws.secretKey", "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");
    }

    @BeforeEach
    void setUpBeforeEach() {
        Message message = new Message()
                .withBody(new Body().withHtml(new Content().withCharset("UTF-8").withData("BotDesk. Body of message. "
                        + "Тест сообщения"))
                        .withText(new Content().withCharset("UTF-8").withData(" someTestToken45df4sad45f4af4as5f4sfd")))
                .withSubject(new Content().withCharset("UTF-8")
                        .withData("Test message from UnitTest of BotDesk : " + new DateTime().toString()));
        sendEmailRequest = new SendEmailRequest()
                .withDestination(new Destination().withToAddresses(RECIPIENT))
                .withMessage(message);

    }

    @Test
    void sendEmail() {
        String messageId = amazonSES.sendEmail(sendEmailRequest);
        assertNotNull(messageId);
    }

}