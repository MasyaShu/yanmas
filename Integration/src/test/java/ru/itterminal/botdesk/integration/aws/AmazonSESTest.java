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
import com.amazonaws.services.simpleemail.model.Message;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {AmazonSES.class})
@TestPropertySource(properties = {"aws.ses.email.noReplay=noreplay@botdesk.app", "aws.ses.region=eu-central-1"})
class AmazonSESTest {

    @Autowired
    AmazonSES amazonSES;

    private static Message message;
    private static final String testEmail  = "info@it-terminal.ru";

    @BeforeAll
    void setUpBeforeAll() {
        System.setProperty("aws.accessKeyId", "AKIAUUSU2LYV5JFH7WOV");
        System.setProperty("aws.secretKey", "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");
    }

    @BeforeEach
    void setUpBeforeEach() {
        message = new Message()
                .withBody(new Body().withHtml(new Content().withCharset("UTF-8").withData("BotDesk. Body of message. "
                        + "Тест сообщения"))
                        .withText(new Content().withCharset("UTF-8").withData(" someTestToken45df4sad45f4af4as5f4sfd")))
        .withSubject(new Content().withCharset("UTF-8").withData("Test message from UnitTest of BotDesk" + new DateTime().toString()));
    }

    @Test
    void sendEmail() {
        String messageId = amazonSES.sendEmail(message, testEmail);
        assertNotNull(messageId);
    }

}