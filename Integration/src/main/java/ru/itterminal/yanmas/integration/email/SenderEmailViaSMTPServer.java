package ru.itterminal.yanmas.integration.email;

import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_PRODUCTION;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;

import org.simplejavamail.api.email.Email;
import org.simplejavamail.api.mailer.Mailer;
import org.simplejavamail.api.mailer.config.TransportStrategy;
import org.simplejavamail.email.EmailBuilder;
import org.simplejavamail.mailer.MailerBuilder;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import lombok.Getter;

@Component
@Getter
public class SenderEmailViaSMTPServer {

    @Value("${spring.profiles.active}")
    private String activeProfiles;

    @Value("${smtp.server.from}")
    private String smtpServerFrom;

    private final Mailer mailer;

    public SenderEmailViaSMTPServer(@Value("${smtp.server.host}") String smtpServerHost,
                                    @Value("${smtp.server.port}") String smtpServerPort,
                                    @Value("${smtp.server.user}") String smtpServerUser,
                                    @Value("${smtp.server.password}") String smtpServerPassword,
                                    @Value("${smtp.server.debug}") String smtpServerDebug) {
        mailer = MailerBuilder
                .withSMTPServer(smtpServerHost, Integer.valueOf(smtpServerPort), smtpServerUser, smtpServerPassword)
                .withTransportStrategy(TransportStrategy.SMTP)
                .withDebugLogging(Boolean.valueOf(smtpServerDebug))
                .withThreadPoolSize(20) // multi-threaded batch handling
                .withConnectionPoolCoreSize(10) // reusable connection(s) / multi-server sending
                .buildMailer();
    }

    public void sendEmail(Email email) {
        if (activeProfiles.equals(SPRING_ACTIVE_PROFILE_FOR_PRODUCTION) ||
                activeProfiles.equals(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)) {
            mailer.sendMail(email);
        }
    }

    public Email createEmail(String recipient, String subject, String bodyText) {
        return EmailBuilder.startingBlank()
                .from(smtpServerFrom)
                .to(recipient)
                .withSubject(subject)
                .withPlainText(bodyText)
                .buildEmail();
    }

}
