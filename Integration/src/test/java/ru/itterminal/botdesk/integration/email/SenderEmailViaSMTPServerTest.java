package ru.itterminal.botdesk.integration.email;


import java.util.Calendar;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest(classes = {SenderEmailViaSMTPServer.class})
class SenderEmailViaSMTPServerTest {

    @Autowired
    private SenderEmailViaSMTPServer senderEmail;

    @Test
    void sendEmail_shouldSendEmail_whenPassedValidData() {
        var email = senderEmail.createEmail(
                "unit-test@botdesk.app",
                "Test message from UnitTest of BotDesk : " + Calendar.getInstance().getTime().toString(),
                "BotDesk. Тест сообщения"
        );
        senderEmail.sendEmail(email);
    }

}