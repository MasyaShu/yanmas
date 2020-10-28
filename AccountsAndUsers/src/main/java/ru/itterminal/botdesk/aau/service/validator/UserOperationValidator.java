package ru.itterminal.botdesk.aau.service.validator;

import static java.lang.String.format;
import static java.util.Collections.singletonList;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.projection.UserUniqueFields;
import ru.itterminal.botdesk.aau.service.impl.RoleServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.jwt.JwtUser;

@Slf4j
@Component
public class UserOperationValidator extends BasicOperationValidatorImpl<User> {
    private UserServiceImpl service;
    private RoleServiceImpl roleService;

    protected static final String USER_WITH_ROLE_ACCOUNT_OWNER_IS_UNIQUE = "User: {} with role ACCOUNT_OWNER is unique";
    protected static final String USER_WITH_ROLE_ACCOUNT_OWNER = "User with role ACCOUNT_OWNER";
    protected static final String ACCOUNT_MUST_HAVE_USER_WITH_ROLE_ACCOUNT_OWNER =
            "Account must have user with role ACCOUNT_OWNER";
    protected static final String WEIGHT_OF_ROLE_CURRENT_USER_LESS_THAN_WEIGHT_OF_ROLE_FROM_REQUEST = "Weight of role"
            + " current user (%s) less than weight of role from request (%s)";
    protected static final String WEIGHT_OF_ROLE = "Weight of role";

    @Autowired
    public UserOperationValidator(UserServiceImpl service,
            RoleServiceImpl roleService) {
        this.service = service;
        this.roleService = roleService;
    }

    @Override
    public boolean beforeCreate(User entity) {
        super.beforeCreate(entity);
        Map<String, List<ValidationError>> errors = new HashMap<>();

        if (entity.getRole().equals(roleService.getAccountOwnerRole())) {
            List<User> foundUsers = service.findAllByRoleAndAccountId(roleService.getAccountOwnerRole(),
                    entity.getAccount().getId());
            if (foundUsers.isEmpty()) {
                log.trace(USER_WITH_ROLE_ACCOUNT_OWNER_IS_UNIQUE, entity);
            } else {
                errors.put(USER_WITH_ROLE_ACCOUNT_OWNER, singletonList(new ValidationError(NOT_UNIQUE_CODE,
                        format(NOT_UNIQUE_MESSAGE, USER_WITH_ROLE_ACCOUNT_OWNER))));
            }
        }

        if (!SecurityContextHolder.getContext().getAuthentication().getName().equals("anonymousUser")) {
            JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
            if (entity.getRole().getWeight() > jwtUser.getWeightRole()) {
                errors.put(WEIGHT_OF_ROLE, singletonList(new ValidationError(LOGIC_CONSTRAINT_CODE,
                        format(WEIGHT_OF_ROLE_CURRENT_USER_LESS_THAN_WEIGHT_OF_ROLE_FROM_REQUEST, jwtUser, entity))));
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

        // Schema of constraints "Only one user must have a role "Account owner"
        // Entity from DB           | new  Entity               | result
        // has role Acc_Owner       | has role Acc_Owner        | not allowed (User with role ACCOUNT_OWNER is occupied)
        // has not role Acc_Owner   | has role Acc_Owner        | not possible
        // has role Acc_Owner       | has not role Acc_Owner    | all right
        // has not role Acc_Owner   | has not role Acc_Owner    | not allowed, Account must have user with role ACCOUNT_OWNER

        List<User> userFromDatabase = service.findAllByRoleAndIdNot(roleService.getAccountOwnerRole(), entity.getId());
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

        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (entity.getRole().getWeight() > jwtUser.getWeightRole()) {
            errors.put(WEIGHT_OF_ROLE, singletonList(new ValidationError(LOGIC_CONSTRAINT_CODE,
                    format(WEIGHT_OF_ROLE_CURRENT_USER_LESS_THAN_WEIGHT_OF_ROLE_FROM_REQUEST, jwtUser, entity))));
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

}
