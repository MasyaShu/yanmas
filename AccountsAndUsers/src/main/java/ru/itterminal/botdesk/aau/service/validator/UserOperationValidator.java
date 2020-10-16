package ru.itterminal.botdesk.aau.service.validator;

import static java.lang.String.format;
import static java.util.Collections.singletonList;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Component;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.projection.UserUniqueFields;
import ru.itterminal.botdesk.aau.repository.RoleRepository;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;

@Slf4j
@Component
public class UserOperationValidator extends BasicOperationValidatorImpl<User> {
    private UserServiceImpl service;
    private BCryptPasswordEncoder encoder;
    private RoleRepository roleRepository;
    private Role accountOwnerRole;

    protected static final String USER_WITH_ROLE_ACCOUNT_OWNER_IS_UNIQUE = "User: {} with role ACCOUNT_OWNER is unique";
    protected static final String USER_WITH_ROLE_ACCOUNT_OWNER = "User with role ACCOUNT_OWNER";
    protected static final String ACCOUNT_MUST_HAVE_USER_WITH_ROLE_ACCOUNT_OWNER =
            "Account must have user with role ACCOUNT_OWNER";

    @Autowired
    public UserOperationValidator(UserServiceImpl service,
            BCryptPasswordEncoder encoder, RoleRepository roleRepository) {
        this.service = service;
        this.encoder = encoder;
        this.roleRepository = roleRepository;
    }

    @Override
    public boolean beforeCreate(User entity) {
        super.beforeCreate(entity);
        Map<String, List<ValidationError>> errors = new HashMap<>();

        if (entity.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString())) {
            List<User> foundUsers = service.findAllByRole(getAccountOwnerRole());
            if (foundUsers.isEmpty()) {
                log.trace(USER_WITH_ROLE_ACCOUNT_OWNER_IS_UNIQUE, entity);
            } else {
                errors.put(USER_WITH_ROLE_ACCOUNT_OWNER, singletonList(new ValidationError(NOT_UNIQUE_CODE,
                        format(NOT_UNIQUE_MESSAGE, USER_WITH_ROLE_ACCOUNT_OWNER))));
            }
        }

        if (!errors.isEmpty()) {
            log.error(FIELDS_ARE_NOT_VALID, errors);
            throw new LogicalValidationException(VALIDATION_FAILED, errors);
        }
        return true;
    }

    @Override
    public boolean beforeUpdate(User entity) {
        super.beforeUpdate(entity);
        Map<String, List<ValidationError>> errors = new HashMap<>();

        // Entity from DB           | new  Entity               | result
        // has role Acc_Owner       | has role Acc_Owner        | not allowed (User with role ACCOUNT_OWNER is occupied)
        // has not role Acc_Owner   | has role Acc_Owner        | not possible
        // has role Acc_Owner       | has not role Acc_Owner    | all right
        // has not role Acc_Owner   | has not role Acc_Owner    | not allowed, Account must have user with role ACCOUNT_OWNER

        List<User> userFromDatabase = service.findAllByRoleAndIdNot(getAccountOwnerRole(), entity.getId());
        boolean oldUserHasRoleAccountOwner = false;
        if (!userFromDatabase.isEmpty()) {
            oldUserHasRoleAccountOwner = userFromDatabase.get(0).getRole().getName().equals(Roles.ACCOUNT_OWNER.toString());
        }
        boolean newUserHasRoleAccountOwner = entity.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString());

        if (oldUserHasRoleAccountOwner && newUserHasRoleAccountOwner) {
            errors.put(USER_WITH_ROLE_ACCOUNT_OWNER, singletonList(new ValidationError(NOT_UNIQUE_CODE,
                    format(NOT_UNIQUE_MESSAGE, USER_WITH_ROLE_ACCOUNT_OWNER))));
        }

        if (!oldUserHasRoleAccountOwner && !newUserHasRoleAccountOwner) {
            errors.put(USER_WITH_ROLE_ACCOUNT_OWNER, singletonList(new ValidationError(NOT_UNIQUE_CODE,
                    ACCOUNT_MUST_HAVE_USER_WITH_ROLE_ACCOUNT_OWNER)));
        }

        if (!errors.isEmpty()) {
            log.error(FIELDS_ARE_NOT_VALID, errors);
            throw new LogicalValidationException(VALIDATION_FAILED, errors);
        }
        return true;
    }

    @Override
    public boolean checkUniqueness(User entity) {
        log.trace(CHECK_UNIQUENESS, entity);
        Map<String, List<ValidationError>> errors = new HashMap<>();
        List<UserUniqueFields> foundUser = service.findByUniqueFields(entity);
        if (foundUser.isEmpty()) {
            log.trace(FIELDS_UNIQUE, entity);
            return true;
        } else {
            String validatedField;
            if (entity.getEmail().equalsIgnoreCase(foundUser.get(0).getEmail())) {
                validatedField = "email";
                errors.put(validatedField, singletonList(new ValidationError(NOT_UNIQUE_CODE,
                        format(NOT_UNIQUE_MESSAGE, validatedField))));
            }
            log.error(FIELDS_NOT_UNIQUE, errors);
            throw new LogicalValidationException(VALIDATION_FAILED, errors);
        }
    }

    @Override
    public boolean checkLogicalDelete(UUID id) {
        super.checkLogicalDelete(id);
        // TODO chek last user with role SuperAdmin
        return true;
    }

    private Role getAccountOwnerRole() {
        if (accountOwnerRole == null) {
            accountOwnerRole = roleRepository.getByName(Roles.ACCOUNT_OWNER.toString()).get();
        }
        return accountOwnerRole;
    }
}
