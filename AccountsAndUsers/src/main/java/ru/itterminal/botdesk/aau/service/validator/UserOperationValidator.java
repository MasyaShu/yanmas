package ru.itterminal.botdesk.aau.service.validator;

import static java.lang.String.format;
import static java.util.Collections.singletonList;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.projection.UserUniqueFields;
import ru.itterminal.botdesk.aau.service.impl.RoleServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.security.jwt.JwtUser;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserOperationValidator extends BasicOperationValidatorImpl<User> {

    public static final String IS_FORBIDDEN_TO_ASSIGN_A_USER_WITH_A_ROLE = "With your role it is forbidden to assign a user with a role %s";
    public static final String IS_FORBIDDEN_TO_CHANGE_THE_USER_WITH_THE_ROLE = "With your role it is forbidden to change the user with the role %s";
    public static final String NOT_ALLOWED_TO_ASSIGN_A_GROUP_WITH_ID_NOT_EQUAL = "You are not allowed to assign a group with id not equal: %s";
    public static final String CANNOT_EDIT_A_USER_WHO_IS_NOT_IN_THE_GROUP_WITH_ID = "You cannot edit a user who is not in the group with id: %s";
    private final UserServiceImpl service;
    private final RoleServiceImpl roleService;

    protected static final String USER_WITH_ROLE_ACCOUNT_OWNER_IS_UNIQUE = "User: {} with role ACCOUNT_OWNER is unique";
    protected static final String USER_WITH_ROLE_ACCOUNT_OWNER = "User with role ACCOUNT_OWNER";
    protected static final String ACCOUNT_MUST_HAVE_USER_WITH_ROLE_ACCOUNT_OWNER =
            "Account must have user with role ACCOUNT_OWNER";
    protected static final String WEIGHT_OF_ROLE_CURRENT_USER_LESS_THAN_WEIGHT_OF_ROLE_FROM_REQUEST =
            "Weight of role current user (%s) less than weight of role from request (%s)";
    protected static final String WEIGHT_OF_ROLE = "Weight of role";
    protected static final String WEIGHT_OF_ROLE_USER_FROM_DATABASE = "Weight of role of user from database";
    protected static final String INNER_GROUP = "Inner group";
    protected static final String INNER_GROUP_USER_FROM_DATABASE = "Inner group of user from database";
    protected static final String CREATE_UPDATE_ONLY_HIS_GROUP =
            "If user is not in inner group, then he can create/update user only in his "
                    + "group";
    public static final String ACCESS_IS_DENIED_FOR_SEARCHING_BY_PASSED_ID = "Access is denied for searching by passed Id";
    private static final String EMAIL = "email";

    @Override
    public boolean beforeCreate(User entity) {
        super.beforeCreate(entity);
        if (!SecurityContextHolder.getContext().getAuthentication().getName().contains("anonymous")) {
            checkAccessCreateUpdate(entity, null);
        }
        checkAccountOwnerNotExist(entity);
        return true;
    }

    @Override
    public boolean beforeUpdate(User entity) {
        super.beforeUpdate(entity);
        User userFromDatabase = service.findById(entity.getId());
        checkAccessCreateUpdate(entity, userFromDatabase);
        checkAccountOwnerExistsAfterUpdate(entity);
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
                validatedField = EMAIL;
                errors.put(validatedField, singletonList(new ValidationError(NOT_UNIQUE_CODE,
                        format(NOT_UNIQUE_MESSAGE, validatedField))));
            }
            log.error(FIELDS_NOT_UNIQUE, errors);
            throw new LogicalValidationException(VALIDATION_FAILED, errors);
        }
    }

    @Override
    public boolean checkAccessForRead(User entity) {
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (jwtUser.isInnerGroup() || jwtUser.getGroupId().equals(entity.getGroup().getId())) {
            return true;
        } else {
            throw new AccessDeniedException(ACCESS_IS_DENIED_FOR_SEARCHING_BY_PASSED_ID);
        }
    }

    private void checkAccessCreateUpdate(User userFromRequest, User userFromDatabase) {
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (userFromRequest.getRole().getWeight() > jwtUser.getWeightRole()) {
            throw new AccessDeniedException(format(IS_FORBIDDEN_TO_ASSIGN_A_USER_WITH_A_ROLE,
                    userFromRequest.getRole().getDisplayName()));
        }
        if (userFromDatabase != null && userFromDatabase.getRole().getWeight() > jwtUser.getWeightRole()) {
            throw new AccessDeniedException(format(IS_FORBIDDEN_TO_CHANGE_THE_USER_WITH_THE_ROLE,
                    userFromRequest.getRole().getDisplayName()));
        }
        if (!jwtUser.isInnerGroup() && !userFromRequest.getGroup().getId().equals(jwtUser.getGroupId())) {
            throw new AccessDeniedException(format(NOT_ALLOWED_TO_ASSIGN_A_GROUP_WITH_ID_NOT_EQUAL,
                    jwtUser.getGroupId()));
        }
        if (userFromDatabase != null && !jwtUser.isInnerGroup() && !userFromDatabase.getGroup().getId().equals(jwtUser.getGroupId())) {
            throw new AccessDeniedException(format(CANNOT_EDIT_A_USER_WHO_IS_NOT_IN_THE_GROUP_WITH_ID,
                    jwtUser.getGroupId()));
        }
    }

    private void checkAccountOwnerNotExist(User entity) {
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

        if (!errors.isEmpty()) {
            log.error(FIELDS_ARE_NOT_VALID, errors);
            throw new LogicalValidationException(VALIDATION_FAILED, errors);
        }
    }

    private void checkAccountOwnerExistsAfterUpdate(User entity) {

        // Schema of constraints "Only one user must have a role "Account owner"
        // Entity from DB           | new  Entity               | result
        // has role Acc_Owner       | has role Acc_Owner        | not allowed (User with role ACCOUNT_OWNER is occupied)
        // has not role Acc_Owner   | has role Acc_Owner        | not possible
        // has role Acc_Owner       | has not role Acc_Owner    | all right
        // has not role Acc_Owner   | has not role Acc_Owner    | not allowed, Account must have user with role ACCOUNT_OWNER

        Map<String, List<ValidationError>> errors = new HashMap<>();
        List<User> usersFromDatabaseWithRoleAccountOwner = service.findAllByRoleAndAccount_IdAndIdNot(
                roleService.getAccountOwnerRole(), entity.getAccount().getId(), entity.getId()
        );

        boolean oldUserHasRoleAccountOwner = !usersFromDatabaseWithRoleAccountOwner.isEmpty();
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
    }

}
