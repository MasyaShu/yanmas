package ru.itterminal.botdesk.integration.aws.ses;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.IOException;
import java.util.Calendar;

import javax.mail.MessagingException;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.integration.aws.AwsConfig;
import software.amazon.awssdk.services.ses.model.SendRawEmailRequest;

@Slf4j
@SpringBootTest(classes = {AwsConfig.class, AwsSesConfig.class, SenderEmailViaAwsSes.class})
class SenderEmailViaAwsSesTest {

    @Autowired
    private SenderEmailViaAwsSes senderEmailViaAwsSes;

    @Test
    void sendEmail_shouldSendEmail_whenPassedValidSendRawEmailRequest() {
        SendRawEmailRequest email = null;
        try {
            email = senderEmailViaAwsSes.createEmail
                    (
                            null,
                            "unit-test@botdesk.app",
                            "Test message from UnitTest of BotDesk : "
                                    + Calendar.getInstance().getTime().toString(),
                            "BotDesk. Тест сообщения",
                            "BotDesk. Тест сообщения"
                    );
        }
        catch (MessagingException | IOException e) {
            log.error(e.getMessage());
        }
        String messageId = senderEmailViaAwsSes.sendEmail(email);
        assertNotNull(messageId);
    }

    @Test
    void sendEmail_shouldSendEmail_whenPassedInvalidRecipientEmail() {
        SendRawEmailRequest email = null;
        try {
            email = senderEmailViaAwsSes.createEmail
                    (
                            null,
                            "not-exist@botdesk.app",
                            "Test message from UnitTest of BotDesk : "
                                    + Calendar.getInstance().getTime().toString(),
                            "BotDesk. Тест сообщения",
                            "BotDesk. Тест сообщения"
                    );
        }
        catch (MessagingException | IOException e) {
            log.error(e.getMessage());
        }
        String messageId = senderEmailViaAwsSes.sendEmail(email);
        assertNotNull(messageId);
    }

}