package ru.itterminal.yanmas.aau.service.impl;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.ifErrorsNotEmptyThrowLogicalValidationException;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import io.jsonwebtoken.JwtException;
import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.repository.UserRepository;
import ru.itterminal.yanmas.aau.service.CrudServiceWithBusinessHandlerImpl;
import ru.itterminal.yanmas.aau.service.validator.user.logical_validation_before_update.CheckUniquesUserBeforeUpdateValidator;
import ru.itterminal.yanmas.commons.exception.EntityNotExistException;
import ru.itterminal.yanmas.commons.exception.FailedSaveEntityException;
import ru.itterminal.yanmas.integration.across_modules.CompletedVerificationAccount;
import ru.itterminal.yanmas.integration.email.SenderEmailViaSMTPServer;
import ru.itterminal.yanmas.security.jwt.JwtProvider;
import ru.itterminal.yanmas.security.jwt.JwtUser;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;

@Service
@RequiredArgsConstructor
public class UserServiceImpl extends CrudServiceWithBusinessHandlerImpl<User, UserRepository> {

    public static final String TICKET_TYPE_SERVICE_IMPL = "ticketTypeServiceImpl";
    public static final String TICKET_STATUS_SERVICE_IMPL = "ticketStatusServiceImpl";

    @Value("${email.VerificationTokenForUpdateEmailOfAccountOwner.subject}")
    private String emailVerificationTokenForUpdateEmailOfAccountOwnerSubject;

    @Value("${email.VerificationTokenForUpdateEmailOfAccountOwner.textBody}")
    private String emailVerificationTokenForUpdateEmailOfAccountOwnerTextBody;

    @Value("${email.PasswordResetToken.subject}")
    private String emailPasswordResetTokenSubject;

    @Value("${email.PasswordResetToken.textBody}")
    private String emailPasswordResetTokenTextBody;

    @Value("${dir.uploaded.files}")
    private String dirUploadedFiles;

    private final BCryptPasswordEncoder encoder;
    private final JwtProvider jwtProvider;
    private final JwtUserBuilder jwtUserBuilder;
    private final SenderEmailViaSMTPServer senderEmailViaSMTPServer;
    private final ApplicationContext appContext;
    private final CheckUniquesUserBeforeUpdateValidator checkUniquesUserBeforeUpdateValidator;

    public static final String NOT_FOUND_USER_BY_RESET_PASSWORD_TOKEN = "Not found user by reset password token";
    public static final String NOT_FOUND_USER_BY_EMAIL_VERIFICATION_TOKEN =
            "Not found user by email verification token";
    public static final String NOT_FOUND_USER_BY_EMAIL = "Not found user by email: %s";
    public static final String FAILED_SAVE_USER_AFTER_VERIFY_EMAIL_TOKEN = "Failed save user after verify email token";
    public static final String FAILED_SAVE_USER_AFTER_RESET_PASSWORD = "Failed save user after reset password";

    @Transactional(readOnly = true)
    public User findByEmail(String email) {
        return repository.getByEmail(email).orElseThrow(
                () -> new EntityNotExistException(
                        format(NOT_FOUND_USER_BY_EMAIL, email)
                )
        );
    }

    @Transactional(readOnly = true, noRollbackForClassName = {"EntityNotExistException"})
    public User findByEmailForCreateAccount(String email) {
        return repository.getByEmail(email).orElseThrow(
                () -> new EntityNotExistException(
                        format(NOT_FOUND_USER_BY_EMAIL, email)
                )
        );
    }

    @Transactional
    public void verifyEmailTokenOfAccountOwner(String token) throws IOException {
        UUID userIdFromJwt = null;
        try {
            if (jwtProvider.validateToken(token)) {
                userIdFromJwt = jwtProvider.getUserId(token);
            }
        }
        catch (Exception e) {
            throw new JwtException(e.getMessage());
        }
        var user = findById(userIdFromJwt);
        if (user.getEmailVerificationToken() == null || !user.getEmailVerificationToken().equals(token)) {
            throw new JwtException(NOT_FOUND_USER_BY_EMAIL_VERIFICATION_TOKEN);
        }
        user.setEmailVerificationStatus(true);
        user.setEmailVerificationToken(null);
        var savedUser = repository.save(user);
        if (savedUser.getEmailVerificationToken() != null
                || !savedUser.getEmailVerificationStatus()) {
            throw new FailedSaveEntityException(FAILED_SAVE_USER_AFTER_VERIFY_EMAIL_TOKEN);
        }
        var folderForUploadFilesForThisAccount = Paths.get(dirUploadedFiles, user.getAccount().getId().toString());
        if (Files.notExists(folderForUploadFilesForThisAccount)) {
            var ticketTypeServiceImpl =
                    (CompletedVerificationAccount) appContext.getBean(TICKET_TYPE_SERVICE_IMPL);
            ticketTypeServiceImpl.actionAfterCompletedVerificationAccount(user.getAccount().getId(), user.getId());

            var ticketStatusServiceImpl =
                    (CompletedVerificationAccount) appContext.getBean(TICKET_STATUS_SERVICE_IMPL);
            ticketStatusServiceImpl.actionAfterCompletedVerificationAccount(user.getAccount().getId(), user.getId());
            Files.createDirectories(folderForUploadFilesForThisAccount);
        }
    }

    @Transactional
    public void requestForResetPassword(String emailOfUser) {
        var user = findByEmail(emailOfUser);
        var token = jwtProvider.createTokenWithUserId(user.getId());
        user.setPasswordResetToken(token);
        repository.save(user);
        var email = senderEmailViaSMTPServer.createEmail(
                emailOfUser,
                emailPasswordResetTokenSubject,
                emailPasswordResetTokenTextBody + " " + token
        );
        senderEmailViaSMTPServer.sendEmail(email);
    }

    @Transactional
    public void requestForUpdateEmailOfAccountOwner(String newEmail, User accountOwner) {
        var accountOwnerWithNewEmail = accountOwner.toBuilder().build();
        accountOwnerWithNewEmail.setEmail(newEmail);
        checkUniquesBeforeUpdate(accountOwnerWithNewEmail);
        var token = jwtProvider.createTokenWithUserEmail(newEmail);
        accountOwner.setEmailVerificationToken(token);
        repository.save(accountOwner);
        var email = senderEmailViaSMTPServer.createEmail(
                newEmail,
                emailVerificationTokenForUpdateEmailOfAccountOwnerSubject,
                emailVerificationTokenForUpdateEmailOfAccountOwnerTextBody + " " + token
        );
        senderEmailViaSMTPServer.sendEmail(email);
    }

    @Transactional
    public void resetPassword(String token, String newPassword) {
        UUID userId = null;
        try {
            if (jwtProvider.validateToken(token)) {
                userId = jwtProvider.getUserId(token);
            }
        }
        catch (Exception e) {
            throw new JwtException(e.getMessage());
        }
        var user = super.findById(userId);
        if (user.getPasswordResetToken() == null || !user.getPasswordResetToken().equals(token)) {
            throw new JwtException(NOT_FOUND_USER_BY_RESET_PASSWORD_TOKEN);
        }
        String encodedNewPassword = encoder.encode(newPassword);
        user.setPassword(encodedNewPassword);
        user.setPasswordResetToken(null);
        var savedUser = repository.save(user);
        if (savedUser.getPassword() == null
                || !encoder.matches(newPassword, savedUser.getPassword())) {
            throw new FailedSaveEntityException(FAILED_SAVE_USER_AFTER_RESET_PASSWORD);
        }
    }

    @Transactional
    public void updateEmailOfAccountOwner(String token, User currentUser) {
        if (!currentUser.getEmailVerificationToken().equals(token)) {
            throw new JwtException(NOT_FOUND_USER_BY_EMAIL_VERIFICATION_TOKEN);
        }
        var newEmail = "";
        try {
            if (jwtProvider.validateToken(token)) {
                newEmail = jwtProvider.getEmail(token);
            }
        }
        catch (Exception e) {
            throw new JwtException(e.getMessage());
        }

        var accountOwnerWithNewEmail = currentUser.toBuilder().build();
        accountOwnerWithNewEmail.setEmail(newEmail);
        checkUniquesBeforeUpdate(accountOwnerWithNewEmail);
        currentUser.setEmailVerificationToken(null);
        currentUser.setEmail(newEmail);
        repository.save(currentUser);
    }

    public JwtUser convertUserToJwtUser(User user) {
        return JwtUser.builder()
                .id(user.getId())
                .accountId(user.getAccount().getId())
                .groupId(user.getGroup().getId())
                .isInnerGroup(user.getGroup().getIsInner())
                .weightRole(user.getRole().getWeight())
                .username(user.getEmail())
                .authorities(List.of(new SimpleGrantedAuthority(user.getRole().getName())))
                .enabled(true)
                .build();
    }

    private void checkUniquesBeforeUpdate(User user) {
        var errors = createMapForLogicalErrors();
        checkUniquesUserBeforeUpdateValidator.logicalValidationBeforeUpdate(user, errors);
        ifErrorsNotEmptyThrowLogicalValidationException(errors);
    }

    @Transactional(readOnly = true)
    public User getCurrentUserFromJwtUser() {
        var jwtUser = jwtUserBuilder.getJwtUser();
        return findById(jwtUser.getId());
    }
}
