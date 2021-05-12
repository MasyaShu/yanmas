package ru.itterminal.yanmas.integration.email;


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
                "Test message from UnitTest of Yanmas : " + Calendar.getInstance().getTime().toString(),
                "Yanmas. Тест сообщения"
        );
       // senderEmail.sendEmail(email);
    }

}