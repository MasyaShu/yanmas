package ru.itterminal.yanmas.aau.service.business_handler.user;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.integration.email.SenderEmailViaSMTPServer;

@Component
@RequiredArgsConstructor
public class SendingEmailWithEmailVerificationTokenAfterCreateUserBusinessHandler implements EntityBusinessHandler<User> {

    private final SenderEmailViaSMTPServer senderEmailViaSMTPServer;


    @Value("${email.VerificationEmailTokenForCreateAccount.subject}")
    private String emailVerificationTokenSubject;

    @Value("${email.VerificationEmailTokenForCreateAccount.textBody}")
    private String emailVerificationTokenTextBody;

    @Override
    public void afterCreate(User entity, User currentUser) {
        if (entity.getEmailVerificationToken() != null) {
            var email = senderEmailViaSMTPServer.createEmail(
                    entity.getEmail(),
                    emailVerificationTokenSubject,
                    emailVerificationTokenTextBody + " " + entity.getEmailVerificationToken()
            );
            senderEmailViaSMTPServer.sendEmail(email);
        }
    }
}
