package ru.itterminal.yanmas.aau.service.impl;

import static java.lang.String.format;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.UUID;

import javax.persistence.OptimisticLockException;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import io.jsonwebtoken.JwtException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.yanmas.aau.model.Role;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.model.projection.UserUniqueFields;
import ru.itterminal.yanmas.aau.repository.UserRepository;
import ru.itterminal.yanmas.aau.service.validator.UserOperationValidator;
import ru.itterminal.yanmas.commons.exception.EntityNotExistException;
import ru.itterminal.yanmas.commons.exception.FailedSaveEntityException;
import ru.itterminal.yanmas.integration.across_modules.CompletedVerificationAccount;
import ru.itterminal.yanmas.integration.email.SenderEmailViaSMTPServer;
import ru.itterminal.yanmas.security.jwt.JwtProvider;
import ru.itterminal.yanmas.security.jwt.JwtUser;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;

@Slf4j
@Service
@RequiredArgsConstructor
public class UserServiceImpl extends CrudServiceWithAccountImpl<User, UserOperationValidator, UserRepository> {

    public static final String TICKET_TYPE_SERVICE_IMPL = "ticketTypeServiceImpl";
    public static final String TICKET_STATUS_SERVICE_IMPL = "ticketStatusServiceImpl";
    public static final String START_UPDATE_EMAIL_FOR_ACCOUNT_OWNER = "Start update email for Account Owner: {}";
    public static final String START_UPDATE_EMAIL_OF_ACCOUNT_OWNER_BY_TOKEN =
            "Start update email of account owner by token: {}";

    @Value("${email.VerificationEmailTokenForCreateAccount.subject}")
    private String emailVerificationTokenSubject;

    @Value("${email.VerificationEmailTokenForCreateAccount.textBody}")
    private String emailVerificationTokenTextBody;

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
    private final RoleServiceImpl roleService;
    private final SenderEmailViaSMTPServer senderEmailViaSMTPServer;
    private final ApplicationContext appContext;

    public static final String START_REQUEST_PASSWORD_RESET_BY_EMAIL = "Start request password reset by email: {}";
    public static final String NOT_FOUND_USER_BY_RESET_PASSWORD_TOKEN = "Not found user by reset password token";
    public static final String NOT_FOUND_USER_BY_EMAIL_VERIFICATION_TOKEN =
            "Not found user by email verification token";
    public static final String START_RESET_PASSWORD_BY_TOKEN_AND_NEW_PASSWORD =
            "Start reset password by token: {} and new password {}";
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

    @Override
    @Transactional
    public User create(User entity) {
        validator.checkAccessBeforeCreate(entity);
        validator.logicalValidationBeforeCreate(entity);
        log.trace(format(CREATE_INIT_MESSAGE, entity.getClass().getSimpleName(), entity.toString()));
        UUID id = UUID.randomUUID();
        entity.setId(id);
        entity.setIsArchived(false);
        validator.checkUniqueness(entity);
        entity.setPassword(encoder.encode(entity.getPassword()));
        if (entity.getRole().equals(roleService.getAccountOwnerRole())) {
            String emailVerificationToken = jwtProvider.createTokenWithUserId(entity.getId());
            entity.setEmailVerificationToken(emailVerificationToken);
            entity.setEmailVerificationStatus(false);
        } else {
            entity.setEmailVerificationStatus(true);
        }
        entity.generateDisplayName();
        User createdUser = repository.create(entity);
        log.trace(format(CREATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), createdUser.toString()));
        if (createdUser.getEmailVerificationToken() != null) {
            var email = senderEmailViaSMTPServer.createEmail(
                    createdUser.getEmail(),
                    emailVerificationTokenSubject,
                    emailVerificationTokenTextBody + " " + createdUser.getEmailVerificationToken()
            );
            senderEmailViaSMTPServer.sendEmail(email);
        }
        return createdUser;
    }

    @Override
    @Transactional
    public User update(User entity) {
        validator.checkAccessBeforeUpdate(entity);
        validator.logicalValidationBeforeUpdate(entity);
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

    @Transactional(readOnly = true, noRollbackForClassName = {"EntityNotExistException"})
    public User findByEmailForCreateAccount(String email) {
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

    @Transactional
    public void verifyEmailTokenOfAccountOwner(String token) throws IOException {
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
        var folderForUploadFilesForThisAccount = Paths.get(dirUploadedFiles, user.getAccount().getId().toString());
        if (Files.notExists(folderForUploadFilesForThisAccount)) {
            var ticketTypeServiceImpl =
                    (CompletedVerificationAccount) appContext.getBean(TICKET_TYPE_SERVICE_IMPL);
            ticketTypeServiceImpl.actionAfterCompletedVerificationAccount(user.getAccount().getId());

            var ticketStatusServiceImpl =
                    (CompletedVerificationAccount) appContext.getBean(TICKET_STATUS_SERVICE_IMPL);
            ticketStatusServiceImpl.actionAfterCompletedVerificationAccount(user.getAccount().getId());
            Files.createDirectories(folderForUploadFilesForThisAccount);
        }
    }

    @SuppressWarnings("DuplicatedCode")
    @Transactional
    public void requestPasswordReset(String emailOfUser) {
        log.trace(START_REQUEST_PASSWORD_RESET_BY_EMAIL, emailOfUser);
        User user = findByEmail(emailOfUser);
        String token = jwtProvider.createTokenWithUserId(user.getId());
        user.setPasswordResetToken(token);
        repository.save(user);
        var email = senderEmailViaSMTPServer.createEmail(
                emailOfUser,
                emailPasswordResetTokenSubject,
                emailPasswordResetTokenTextBody + " " + token
        );
        senderEmailViaSMTPServer.sendEmail(email);
    }

    @SuppressWarnings("DuplicatedCode")
    @Transactional
    public void requestUpdateEmailOfAccountOwner(String newEmail) {
        log.trace(START_UPDATE_EMAIL_FOR_ACCOUNT_OWNER, newEmail);
        var jwtUser = jwtUserBuilder.getJwtUser();
        var accountOwner = super.findByIdAndAccountId(jwtUser.getId());
        var accountOwnerNewEmail = accountOwner.toBuilder().build();
        accountOwnerNewEmail.setEmail(newEmail);
        accountOwnerNewEmail.setId(UUID.randomUUID());
        validator.checkUniqueness(accountOwnerNewEmail);
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

    @Transactional
    public void updateEmailOfAccountOwner(String token) {
        log.trace(START_UPDATE_EMAIL_OF_ACCOUNT_OWNER_BY_TOKEN, token);
        var jwtUser = jwtUserBuilder.getJwtUser();
        var accountOwner = findByIdAndAccountId(jwtUser.getId());
        if (!accountOwner.getEmailVerificationToken().equals(token)) {
            throw new JwtException(NOT_FOUND_USER_BY_EMAIL_VERIFICATION_TOKEN);
        }
        String newEmail = "";
        try {
            if (jwtProvider.validateToken(token)) {
                newEmail = jwtProvider.getEmail(token);
            }
        }
        catch (Exception e) {
            throw new JwtException(e.getMessage());
        }

        var accountOwnerNewEmail = accountOwner.toBuilder().build();
        accountOwnerNewEmail.setId(UUID.randomUUID());
        accountOwnerNewEmail.setEmail(newEmail);
        validator.checkUniqueness(accountOwnerNewEmail);
        accountOwner.setEmailVerificationToken(null);
        accountOwner.setEmail(newEmail);
        repository.save(accountOwner);
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
}
