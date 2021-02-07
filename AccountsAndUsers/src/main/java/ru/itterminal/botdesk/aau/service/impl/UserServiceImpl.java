package ru.itterminal.botdesk.aau.service.impl;

import static java.lang.String.format;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

import javax.mail.MessagingException;
import javax.persistence.OptimisticLockException;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import io.jsonwebtoken.JwtException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.projection.UserUniqueFields;
import ru.itterminal.botdesk.aau.repository.UserRepository;
import ru.itterminal.botdesk.aau.service.validator.UserOperationValidator;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.exception.FailedSaveEntityException;
import ru.itterminal.botdesk.integration.across_modules.CompletedVerificationAccount;
import ru.itterminal.botdesk.integration.aws.ses.SenderEmailViaAwsSes;
import ru.itterminal.botdesk.security.jwt.JwtProvider;
import software.amazon.awssdk.services.ses.model.SendRawEmailRequest;

@Slf4j
@Service
@Transactional
@RequiredArgsConstructor
public class UserServiceImpl extends CrudServiceWithAccountImpl<User, UserOperationValidator, UserRepository> {

    @Value("${emailVerificationToken.subject}")
    private String emailVerificationTokenSubject;

    @Value("${emailVerificationToken.textBody}")
    private String emailVerificationTokenTextBody;

    @Value("${emailPasswordResetToken.subject}")
    private String emailPasswordResetTokenSubject;

    @Value("${emailPasswordResetToken.textBody}")
    private String emailPasswordResetTokenTextBody;

    private final BCryptPasswordEncoder encoder;
    private final JwtProvider jwtProvider;
    private final RoleServiceImpl roleService;
    private final SenderEmailViaAwsSes senderEmailViaAwsSes;
    private final ApplicationContext appContext;

    public static final String START_REQUEST_PASSWORD_RESET_BY_EMAIL = "Start request password reset by email: {}";
    public static final String NOT_FOUND_USER_BY_RESET_PASSWORD_TOKEN = "Not found user by reset password token";
    public static final String NOT_FOUND_USER_BY_EMAIL_VERIFICATION_TOKEN =
            "Not found user by email verification token";
    public static final String START_RESET_PASSWORD_BY_TOKEN_AND_NEW_PASSWORD =
            "Start reset password by token: {} and new password {}";
    public static final String NOT_FOUND_USER_BY_ID_AND_ACCOUNT_ID_AND_OWN_GROUP_ID
            = "Not found user by id: %s and account id: %s and own group id %s";
    public static final String START_FIND_USER_BY_EMAIL = "Start find user by email: {}";
    public static final String NOT_FOUND_USER_BY_EMAIL = "Not found user by email: %s";
    public static final String START_FIND_USER_BY_UNIQUE_FIELDS =
            "Start find user by unique fields, email: {} and not id: {}";
    public static final String START_FIND_ALL_USERS_BY_ROLE_AND_NOT_ID =
            "Start find all users by role: {} and not id: {}";
    public static final String START_FIND_ALL_USERS_BY_ROLE_ACCOUNT_ID_AND_NOT_ID =
            "Start find all users by role: {}, accountId: {} and not id: {}";
    public static final String START_FIND_ALL_USERS_BY_ROLE = "Start find all users by role: {}";
    public static final String START_VERIFY_EMAIL_TOKEN = "Start verify email token: {}";
    public static final String FAILED_SAVE_USER_AFTER_VERIFY_EMAIL_TOKEN = "Failed save user after verify email token";
    public static final String FAILED_SAVE_USER_AFTER_RESET_PASSWORD = "Failed save user after reset password";
    public static final String EMAIL_VERIFICATION_TOKEN_WAS_SUCCESSFUL_SENT = "Email with emailVerificationToken was "
            + "successful sent";
    public static final String RESET_TOKEN_WAS_SUCCESSFUL_SENT = "Email with token for reset password was "
            + "successful sent";

    @Override
    public User create(User entity) {
        validator.beforeCreate(entity);
        log.trace(format(CREATE_INIT_MESSAGE, entity.getClass().getSimpleName(), entity.toString()));
        UUID id = UUID.randomUUID();
        entity.setId(id);
        entity.setIsArchived(false);
        validator.checkUniqueness(entity);
        entity.setPassword(encoder.encode(entity.getPassword()));
        if (entity.getRole().equals(roleService.getAccountOwnerRole())) {
            String emailVerificationToken = jwtProvider.createToken(entity.getId());
            entity.setEmailVerificationToken(emailVerificationToken);
            entity.setEmailVerificationStatus(false);
        } else {
            entity.setEmailVerificationStatus(true);
        }
        entity.generateDisplayName();
        User createdUser = repository.create(entity);
        log.trace(format(CREATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), createdUser.toString()));
        if (createdUser.getEmailVerificationToken() != null) {
            SendRawEmailRequest rawEmailRequest = null;
            try {
                rawEmailRequest = senderEmailViaAwsSes.createEmail(
                        null,
                        createdUser.getEmail(),
                        emailVerificationTokenSubject,
                        emailVerificationTokenTextBody + " " + createdUser.getEmailVerificationToken(),
                        emailVerificationTokenTextBody + " " + createdUser.getEmailVerificationToken()
                );
            }
            catch (MessagingException | IOException e) {
                log.warn(e.getMessage());
            }
            try {
                senderEmailViaAwsSes.sendEmail(rawEmailRequest);
                log.trace(EMAIL_VERIFICATION_TOKEN_WAS_SUCCESSFUL_SENT);
            }
            catch (Exception e) {
                log.warn(e.getMessage());
            }
        }
        return createdUser;
    }

    @Override
    public User update(User entity) {
        validator.beforeUpdate(entity);
        log.trace(format(UPDATE_INIT_MESSAGE, entity.getClass().getSimpleName(), entity.getId(), entity));
        User entityFromDatabase = super.findByIdAndAccountId(entity.getId());
        if (entity.getPassword() == null || entity.getPassword().isEmpty()) {
            entity.setPassword(entityFromDatabase.getPassword());
        } else {
            entity.setPassword(encoder.encode(entity.getPassword()));
        }
        entity.setEmailVerificationStatus(entityFromDatabase.getEmailVerificationStatus());
        entity.setEmailVerificationToken(entityFromDatabase.getEmailVerificationToken());
        entity.setPasswordResetToken(entityFromDatabase.getPasswordResetToken());
        try {
            entity.generateDisplayName();
            User updatedEntity = repository.update(entity);
            log.trace(format(UPDATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), entity.getId(), updatedEntity));
            return updatedEntity;
        }
        catch (OptimisticLockException | ObjectOptimisticLockingFailureException ex) {
            throw new OptimisticLockingFailureException(format(VERSION_INVALID_MESSAGE, entity.getId()));
        }
    }

    @Transactional(readOnly = true)
    public User findByEmail(String email) {
        log.trace(START_FIND_USER_BY_EMAIL, email);
        return repository.getByEmail(email).orElseThrow(
                () -> new EntityNotExistException(
                        format(NOT_FOUND_USER_BY_EMAIL, email)
                )
        );
    }

    @Transactional(readOnly = true)
    public List<UserUniqueFields> findByUniqueFields(User user) {
        log.trace(START_FIND_USER_BY_UNIQUE_FIELDS, user.getEmail(), user.getId());
        return repository.getByEmailAndIdNot(user.getEmail(), user.getId());
    }

    @SuppressWarnings("unused")
    @Transactional(readOnly = true)
    public List<User> findAllByRoleAndIdNot(Role role, UUID id) {
        log.trace(START_FIND_ALL_USERS_BY_ROLE_AND_NOT_ID, role, id);
        return repository.findAllByRoleAndIdNot(role, id);
    }

    @SuppressWarnings("DuplicatedCode")
    @Transactional(readOnly = true)
    public List<User> findAllByRoleAndAccount_IdAndIdNot(Role role, UUID accountId, UUID id) {
        log.trace(START_FIND_ALL_USERS_BY_ROLE_ACCOUNT_ID_AND_NOT_ID, role, accountId, id);
        return repository.findAllByRoleAndAccount_IdAndIdNot(role, accountId, id);
    }

    @SuppressWarnings("unused")
    @Transactional(readOnly = true)
    public List<User> findAllByRole(Role role) {
        log.trace(START_FIND_ALL_USERS_BY_ROLE, role);
        return repository.findAllByRole(role);
    }

    @Transactional(readOnly = true)
    public List<User> findAllByRoleAndAccountId(Role role, UUID accountId) {
        log.trace(START_FIND_ALL_USERS_BY_ROLE, role);
        return repository.findAllByRoleAndAccount_Id(role, accountId);
    }

    public void verifyEmailToken(String token) {
        log.trace(START_VERIFY_EMAIL_TOKEN, token);
        UUID userId = null;
        try {
            if (jwtProvider.validateToken(token)) {
                userId = jwtProvider.getUserId(token);
            }
        }
        catch (Exception e) {
            throw new JwtException(e.getMessage());
        }
        User user = super.findById(userId);
        if (user.getEmailVerificationToken() == null || !user.getEmailVerificationToken().equals(token)) {
            throw new JwtException(NOT_FOUND_USER_BY_EMAIL_VERIFICATION_TOKEN);
        }
        user.setEmailVerificationStatus(true);
        user.setEmailVerificationToken(null);
        User savedUser = repository.save(user);
        if (savedUser.getEmailVerificationToken() != null
                || !savedUser.getEmailVerificationStatus()) {
            throw new FailedSaveEntityException(FAILED_SAVE_USER_AFTER_VERIFY_EMAIL_TOKEN);
        }
        var ticketTypeServiceImpl =
                (CompletedVerificationAccount) appContext.getBean("ticketTypeServiceImpl");
        ticketTypeServiceImpl.actionAfterCompletedVerificationAccount(user.getAccount().getId());

        var ticketStatusServiceImpl =
                (CompletedVerificationAccount) appContext.getBean("ticketStatusServiceImpl");
        ticketStatusServiceImpl.actionAfterCompletedVerificationAccount(user.getAccount().getId());
    }

    public void requestPasswordReset(String email) {
        log.trace(START_REQUEST_PASSWORD_RESET_BY_EMAIL, email);
        User user = findByEmail(email);
        String token = jwtProvider.createToken(user.getId());
        user.setPasswordResetToken(token);
        repository.save(user);
        SendRawEmailRequest rawEmailRequest = null;
        try {
            rawEmailRequest = senderEmailViaAwsSes.createEmail(
                    null,
                    email,
                    emailPasswordResetTokenSubject,
                    emailPasswordResetTokenTextBody + " " + token,
                    emailPasswordResetTokenTextBody + " " + token
            );
        }
        catch (MessagingException | IOException e) {
            log.warn(e.getMessage());
        }
        try {
            senderEmailViaAwsSes.sendEmail(rawEmailRequest);
            log.trace(RESET_TOKEN_WAS_SUCCESSFUL_SENT);
        }
        catch (Exception e) {
            log.warn(e.getMessage());
        }
    }

    public void resetPassword(String token, String newPassword) {
        log.trace(START_RESET_PASSWORD_BY_TOKEN_AND_NEW_PASSWORD, token, newPassword);
        UUID userId = null;
        try {
            if (jwtProvider.validateToken(token)) {
                userId = jwtProvider.getUserId(token);
            }
        }
        catch (Exception e) {
            throw new JwtException(e.getMessage());
        }
        User user = super.findById(userId);
        if (user.getPasswordResetToken() == null || !user.getPasswordResetToken().equals(token)) {
            throw new JwtException(NOT_FOUND_USER_BY_RESET_PASSWORD_TOKEN);
        }
        String encodedNewPassword = encoder.encode(newPassword);
        user.setPassword(encodedNewPassword);
        user.setPasswordResetToken(null);
        User savedUser = repository.save(user);
        if (savedUser.getPassword() == null
                || !encoder.matches(newPassword, savedUser.getPassword())) {
            throw new FailedSaveEntityException(FAILED_SAVE_USER_AFTER_RESET_PASSWORD);
        }
    }
}
