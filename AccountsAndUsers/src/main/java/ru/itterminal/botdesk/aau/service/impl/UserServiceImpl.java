package ru.itterminal.botdesk.aau.service.impl;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.util.CommonConstants.NOT_FOUND_ENTITY_BY_ID_AND_ACCOUNT_ID;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import io.jsonwebtoken.JwtException;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.projection.UserUniqueFields;
import ru.itterminal.botdesk.aau.repository.UserRepository;
import ru.itterminal.botdesk.aau.service.validator.UserOperationValidator;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.exception.FailedSaveEntityException;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceImpl;
import ru.itterminal.botdesk.jwt.JwtProvider;

@Slf4j
@Service
@Transactional
public class UserServiceImpl extends CrudServiceImpl<User, UserOperationValidator, UserRepository> {

    private final BCryptPasswordEncoder encoder;
    private final JwtProvider jwtProvider;
    private final RoleServiceImpl roleService;

    @Autowired
    public UserServiceImpl(BCryptPasswordEncoder encoder, JwtProvider jwtProvider,
            RoleServiceImpl roleService) {
        this.encoder = encoder;
        this.jwtProvider = jwtProvider;
        this.roleService = roleService;
    }

    public static final String START_FIND_USER_BY_ID_AND_ACCOUNT_ID = "Start find user by id: {} and accountId: {}";
    public static final String START_FIND_USER_BY_ID_AND_ACCOUNT_ID_AND_OWN_GROUP_ID
            = "Start find user by id: {} and accountId: {} and own group id {}";
    public static final String START_REQUEST_PASSWORD_RESET_BY_EMAIL = "Start request password reset by email: {}";
    public static final String NOT_FOUND_USER_BY_RESET_PASSWORD_TOKEN = "Not found user by reset password token";
    public static final String NOT_FOUND_USER_BY_EMAIL_VERIFICATION_TOKEN =
            "Not found user by email verification token";
    public static final String START_RESET_PASSWORD_BY_TOKEN_AND_NEW_PASSWORD =
            "Start reset password by token: {} and new password {}";
    public static final String NOT_FOUND_USER_BY_ID_AND_ACCOUNT_ID_AND_OWN_GROUP_ID
            = "Not found user by id: %s and account id: %s and own group id %s";
    public static final String START_FIND_USER_BY_EMAIL = "Start find user by email: {}";
    public static final String NOT_FOUND_USER_BY_EMAIL_EMAIL_IS_NULL = "Not found user by email, email is null";
    public static final String NOT_FOUND_USER_BY_EMAIL_EMAIL_IS_EMPTY = "Not found user by email, email is empty";
    public static final String NOT_FOUND_USER_BY_EMAIL = "Not found user by email: %s";
    public static final String START_FIND_USER_BY_UNIQUE_FIELDS =
            "Start find user by unique fields, email: {} and not id: {}";
    public static final String NOT_FOUND_USERS_BY_UNIQUE_FIELDS_USER_IS_NULL =
            "Not found users by unique fields, user is null";
    public static final String NOT_FOUND_USERS_BY_UNIQUE_FIELDS_EMAIL_IS_NULL =
            "Not found users by unique fields, email is null";
    public static final String NOT_FOUND_USERS_BY_UNIQUE_FIELDS_ID_IS_NULL =
            "Not found users by unique fields, id is null";
    public static final String START_FIND_ALL_USERS_BY_ROLE_AND_NOT_ID =
            "Start find all users by role: {} and not id: {}";
    public static final String NOT_FOUND_USERS_BY_ROLE_AND_NOT_ID = "Not found users by role: %s and not id: %s";
    public static final String START_FIND_ALL_USERS_BY_ROLE = "Start find all users by role: {}";
    public static final String NOT_FOUND_USERS_ROLE_IS_NULL = "Not found users, role is null";
    public static final String NOT_FOUND_USERS_ACCOUNT_ID_IS_NULL = "Not found users, accountId is null";
    public static final String START_VERIFY_EMAIL_TOKEN = "Start verify email token: {}";
    public static final String FAILED_SAVE_USER_AFTER_VERIFY_EMAIL_TOKEN = "Failed save user after verify email token";
    public static final String FAILED_SAVE_USER_AFTER_RESET_PASSWORD = "Failed save user after reset password";

    public final String ENTITY_USER_NAME = User.class.getSimpleName();

    @Override
    public User create(User entity) {
        validator.beforeCreate(entity);
        log.trace(format(CREATE_INIT_MESSAGE, entity.getClass().getSimpleName(), entity.toString()));
        UUID id = UUID.randomUUID();
        entity.setId(id);
        entity.setIsArchived(false);
        entity.setDeleted(false);
        validator.checkUniqueness(entity);
        entity.setPassword(encoder.encode(entity.getPassword()));
        if (entity.getRole().equals(roleService.getAccountOwnerRole())) {
            String emailVerificationToken = jwtProvider.createToken(entity.getId());
            entity.setEmailVerificationToken(emailVerificationToken);
            entity.setEmailVerificationStatus(false);
        } else {
            entity.setEmailVerificationStatus(true);
        }
        User createdUser = repository.create(entity);
        log.trace(format(CREATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), createdUser.toString()));
        //noinspection StatementWithEmptyBody
        if (createdUser.getEmailVerificationToken() != null) {
            // TODO send email: emailVerificationToken
        }
        return createdUser;
    }

    @Override
    public User update(User entity) {
        validator.beforeUpdate(entity);
        validator.checkUniqueness(entity);
        log.trace(format(UPDATE_INIT_MESSAGE, entity.getClass().getSimpleName(), entity.getId(), entity));
        User entityFromDatabase = super.findById(entity.getId());
        if (!entity.getPassword().isEmpty()) {
            entity.setPassword(encoder.encode(entity.getPassword()));
        } else {
            entity.setPassword(entityFromDatabase.getPassword());
        }

        try {
            User updatedEntity = repository.update(entity);
            log.trace(format(UPDATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), entity.getId(), updatedEntity));
            return updatedEntity;
        }
        catch (ObjectOptimisticLockingFailureException ex) {
            throw new OptimisticLockingFailureException(format(VERSION_INVALID_MESSAGE, entity.getId()));
        }
    }

    @Transactional(readOnly = true)
    public Optional<User> findByEmail(String email) {
        if (email == null) {
            log.error(NOT_FOUND_USER_BY_EMAIL_EMAIL_IS_NULL);
            throw new EntityNotExistException(NOT_FOUND_USER_BY_EMAIL_EMAIL_IS_NULL);
        }
        if (email.isEmpty()) {
            log.error(NOT_FOUND_USER_BY_EMAIL_EMAIL_IS_EMPTY);
            throw new EntityNotExistException(NOT_FOUND_USER_BY_EMAIL_EMAIL_IS_EMPTY);
        }
        log.trace(START_FIND_USER_BY_EMAIL, email);
        return repository.getByEmail(email);
    }

    @Transactional(readOnly = true)
    public List<UserUniqueFields> findByUniqueFields(User user) {
        if (user == null) {
            log.error(NOT_FOUND_USERS_BY_UNIQUE_FIELDS_USER_IS_NULL);
            throw new EntityNotExistException(NOT_FOUND_USERS_BY_UNIQUE_FIELDS_USER_IS_NULL);
        }
        if (user.getEmail() == null) {
            log.error(NOT_FOUND_USERS_BY_UNIQUE_FIELDS_EMAIL_IS_NULL);
            throw new EntityNotExistException(NOT_FOUND_USERS_BY_UNIQUE_FIELDS_EMAIL_IS_NULL);
        }
        if (user.getId() == null) {
            log.error(NOT_FOUND_USERS_BY_UNIQUE_FIELDS_ID_IS_NULL);
            throw new EntityNotExistException(NOT_FOUND_USERS_BY_UNIQUE_FIELDS_ID_IS_NULL);
        }
        log.trace(START_FIND_USER_BY_UNIQUE_FIELDS, user.getEmail(), user.getId());
        return repository.getByEmailAndIdNot(user.getEmail(), user.getId());
    }

    @Transactional(readOnly = true)
    public User findByIdAndAccountId(UUID id, UUID accountId) {
        checkEntityIdAndAccountId(ENTITY_USER_NAME, id, accountId);
        log.trace(START_FIND_USER_BY_ID_AND_ACCOUNT_ID, id, accountId);
        return repository.getByIdAndAccount_Id(id, accountId).orElseThrow(
                () -> new EntityNotExistException(format(NOT_FOUND_ENTITY_BY_ID_AND_ACCOUNT_ID, ENTITY_USER_NAME, id,
                        accountId))
        );
    }

    @Transactional(readOnly = true)
    public User findByIdAndAccountIdAndOwnGroupId(UUID id, UUID accountId, UUID ownGroupId) {
        checkEntityIdAndAccountId(ENTITY_USER_NAME, id, accountId);
        if (ownGroupId == null) {
            String message = format(NOT_FOUND_USER_BY_ID_AND_ACCOUNT_ID_AND_OWN_GROUP_ID, id, accountId, null);
            log.error(message);
            throw new EntityNotExistException(message);
        }
        log.trace(START_FIND_USER_BY_ID_AND_ACCOUNT_ID_AND_OWN_GROUP_ID, id, accountId, ownGroupId);
        return repository.getByIdAndAccount_IdAndOwnGroup_Id(id, accountId, ownGroupId).orElseThrow(
                () -> new EntityNotExistException(
                        format(NOT_FOUND_USER_BY_ID_AND_ACCOUNT_ID_AND_OWN_GROUP_ID, id, accountId, ownGroupId)
                )
        );
    }

    @Transactional(readOnly = true)
    public List<User> findAllByRoleAndIdNot(Role role, UUID id) {
        if (id == null) {
            String message = format(NOT_FOUND_USERS_BY_ROLE_AND_NOT_ID, role, null);
            log.error(message);
            throw new EntityNotExistException(message);
        }
        if (role == null) {
            String message = format(NOT_FOUND_USERS_BY_ROLE_AND_NOT_ID, null, id);
            log.error(message);
            throw new EntityNotExistException(message);
        }
        log.trace(START_FIND_ALL_USERS_BY_ROLE_AND_NOT_ID, role, id);
        return repository.findAllByRoleAndIdNot(role, id);
    }

    @Transactional(readOnly = true)
    public List<User> findAllByRole(Role role) {
        if (role == null) {
            log.error(NOT_FOUND_USERS_ROLE_IS_NULL);
            throw new EntityNotExistException(NOT_FOUND_USERS_ROLE_IS_NULL);
        }
        log.trace(START_FIND_ALL_USERS_BY_ROLE, role);
        return repository.findAllByRole(role);
    }

    @Transactional(readOnly = true)
    public List<User> findAllByRoleAndAccountId(Role role, UUID accountId) {
        if (role == null) {
            log.error(NOT_FOUND_USERS_ROLE_IS_NULL);
            throw new EntityNotExistException(NOT_FOUND_USERS_ROLE_IS_NULL);
        }
        if (accountId == null) {
            log.error(NOT_FOUND_USERS_ACCOUNT_ID_IS_NULL);
            throw new EntityNotExistException(NOT_FOUND_USERS_ACCOUNT_ID_IS_NULL);
        }
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
        catch (Throwable e) {
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
    }

    public void requestPasswordReset(String email) {
        log.trace(START_REQUEST_PASSWORD_RESET_BY_EMAIL, email);
        User user = findByEmail(email).orElseThrow(
                () -> new EntityNotExistException(format(NOT_FOUND_USER_BY_EMAIL, email)));
        String token = jwtProvider.createToken(user.getId());
        user.setPasswordResetToken(token);
        repository.save(user);
        // TODO send email: PasswordResetToken
    }

    public void resetPassword(String token, String newPassword) {
        log.trace(START_RESET_PASSWORD_BY_TOKEN_AND_NEW_PASSWORD, token, newPassword);
        UUID userId = null;
        try {
            if (jwtProvider.validateToken(token)) {
                userId = jwtProvider.getUserId(token);
            }
        }
        catch (Throwable e) {
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
