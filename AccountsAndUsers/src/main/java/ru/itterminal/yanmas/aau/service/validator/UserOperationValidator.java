package ru.itterminal.yanmas.aau.service.validator;

import static java.lang.String.format;
import static java.util.Collections.singletonList;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.ifErrorsNotEmptyThrowLogicalValidationException;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.context.ApplicationContext;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.repository.UserRepository;
import ru.itterminal.yanmas.aau.service.impl.RoleServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.commons.exception.LogicalValidationException;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.yanmas.integration.across_modules.RequestsFromModuleAccountAndUsers;
import ru.itterminal.yanmas.security.jwt.JwtUser;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserOperationValidator extends BasicOperationValidatorImpl<User> {

    public static final String IS_FORBIDDEN_TO_ASSIGN_A_USER_WITH_A_ROLE =
            "With your role it is forbidden to assign a user with a role %s";
    public static final String IS_FORBIDDEN_TO_CHANGE_THE_USER_WITH_THE_ROLE =
            "With your role it is forbidden to change the user with the role %s";
    public static final String NOT_ALLOWED_TO_ASSIGN_A_GROUP_WITH_ID_NOT_EQUAL =
            "You are not allowed to assign a group with id not equal: %s";
    public static final String CANNOT_EDIT_A_USER_WHO_IS_NOT_IN_THE_GROUP_WITH_ID =
            "You cannot edit a user who is not in the group with id: %s";
    public static final String YOU_CAN_EDIT_EMAIL_OF_THE_USER_WITH_A_ROLE_ACCOUNT_OWNER_ONLY_VIA_SPECIAL_PROCESS =
            "You can edit email of the user with a role Account owner only via special process called \"update email of account owner\"";
    public static final String CHANGE_USER_GROUP = "Change user group";
    public static final String THE_USER_CANNOT_CHANGE_THE_GROUP_BECAUSE_HE_IS_PRESENT_IN_TICKETS =
            "The user cannot change the group, because he is present in tickets";
    public static final String START_FIND_USER_BY_UNIQUE_FIELDS =
            "Start find user by unique fields, email: {} and not id: {}";
    private final UserServiceImpl service;
    private final RoleServiceImpl roleService;
    private final ApplicationContext appContext;
    private final UserRepository repository;

    protected static final String USER_WITH_ROLE_ACCOUNT_OWNER_IS_UNIQUE = "User: {} with role ACCOUNT_OWNER is unique";
    protected static final String USER_WITH_ROLE_ACCOUNT_OWNER = "User with role ACCOUNT_OWNER";
    protected static final String ACCOUNT_MUST_HAVE_USER_WITH_ROLE_ACCOUNT_OWNER =
            "Account must have user with role ACCOUNT_OWNER";
    public static final String ACCESS_IS_DENIED_FOR_SEARCHING_BY_PASSED_ID =
            "Access is denied for searching by passed Id";
    public static final String EMAIL = "email";

    @Deprecated
    @Override
    public void checkAccessBeforeCreate(User entity) {
        if (!SecurityContextHolder.getContext().getAuthentication().getName().contains(ANONYMOUS)) {
            checkAccessCreateUpdate(entity, null);
        }
    }

    @Deprecated
    @Override
    public boolean logicalValidationBeforeCreate(User entity) {
        super.logicalValidationBeforeCreate(entity);
        checkAccountOwnerNotExist(entity);
        return true;
    }

    @Deprecated
    @Override
    public void checkAccessBeforeUpdate(User entity) {
        var userFromDatabase = service.findById(entity.getId());
        checkAccessCreateUpdate(entity, userFromDatabase);
    }

    @Deprecated
    @Override
    public boolean logicalValidationBeforeUpdate(User entity) {
        super.logicalValidationBeforeUpdate(entity);
        var userFromDatabase = service.findById(entity.getId());
        // NotAllowedChangeGroupOfUserIfUserWasUsedInTicketsValidator
        if (!entity.getGroup().equals(userFromDatabase.getGroup())) {
            checkPossibilityOfChangingUserGroup(entity);
        }
        checkAccountOwnerExistsAfterUpdateAndRestrictionForEditEmailOfAccountOwner(entity, userFromDatabase);
        return true;
    }

    @Deprecated
    @Override
    public boolean checkUniqueness(User entity) {
        log.trace(CHECK_UNIQUENESS, entity);
        Map<String, List<ValidationError>> errors = new HashMap<>();
        log.trace(START_FIND_USER_BY_UNIQUE_FIELDS, entity.getEmail(), entity.getId());
        var foundUser = repository.getByEmailAndIdNot(entity.getEmail(), entity.getId());
        if (foundUser.isEmpty()) {
            log.trace(FIELDS_UNIQUE, entity);
            return true;
        } else {
            String validatedField;
            if (entity.getEmail().equalsIgnoreCase(foundUser.get(0).getEmail())) {
                validatedField = EMAIL;
                errors.put(validatedField, singletonList(new ValidationError(
                        NOT_UNIQUE_CODE,
                        format(NOT_UNIQUE_MESSAGE, validatedField)
                )));
            }
            log.error(FIELDS_NOT_UNIQUE, errors);
            throw new LogicalValidationException(VALIDATION_FAILED, errors);
        }
    }

    @Override
    @Deprecated
    public void checkAccessBeforeRead(User entity) {
        if (!SecurityContextHolder.getContext().getAuthentication().getName().contains(ANONYMOUS)) {
            JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
            // AccessDeniedIfCurrentUserFromOuterGroupAndGroupsIdIsNotEqualsValidator
            // AccessDeniedIfWeightOfRoleOfCurrentUserLessThanWeightOfExecutorAndGroupsIdIsNotEqualsValidator
            if (!((jwtUser.isInnerGroup() && jwtUser.getWeightRole() > Roles.AUTHOR.getWeight())
                    || jwtUser.getGroupId().equals(entity.getGroup().getId())
            )
            ) {
                throw new AccessDeniedException(ACCESS_IS_DENIED_FOR_SEARCHING_BY_PASSED_ID);
            }
        }
    }

    @Deprecated
    private void checkAccessCreateUpdate(User userFromRequest, User userFromDatabase) {
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        // WeightOfRoleOfCurrentUserMustBeGreatThanWeightOfRoleFromEntityBeforeCreateUserValidator
        // WeightOfRoleOfCurrentUserMustBeGreatThanWeightOfRoleFromEntityBeforeUpdateUserValidator
        if (userFromRequest.getRole().getWeight() > jwtUser.getWeightRole()) {
            throw new AccessDeniedException(format(
                    IS_FORBIDDEN_TO_ASSIGN_A_USER_WITH_A_ROLE,
                    userFromRequest.getRole().getDisplayName()
            ));
        }
        // WeightOfRoleOfCurrentUserMustBeGreatThanWeightOfRoleFromDatabaseBeforeUpdateUserValidator
        if (userFromDatabase != null && userFromDatabase.getRole().getWeight() > jwtUser.getWeightRole()) {
            throw new AccessDeniedException(format(
                    IS_FORBIDDEN_TO_CHANGE_THE_USER_WITH_THE_ROLE,
                    userFromRequest.getRole().getDisplayName()
            ));
        }
        // UserFromOuterGroupCanCreateUserOnlyWithinHisGroupValidator
        // UserFromOuterGroupCanUpdateUserOnlyWithinHisGroupValidator
        if (!jwtUser.isInnerGroup() && !userFromRequest.getGroup().getId().equals(jwtUser.getGroupId())) {
            throw new AccessDeniedException(format(
                    NOT_ALLOWED_TO_ASSIGN_A_GROUP_WITH_ID_NOT_EQUAL,
                    jwtUser.getGroupId()
            ));
        }
        // UserFromOuterGroupCanUpdateUserOnlyWithinHisGroupValidator
        if (userFromDatabase != null && !jwtUser.isInnerGroup() && !userFromDatabase.getGroup().getId()
                .equals(jwtUser.getGroupId())) {
            throw new AccessDeniedException(format(
                    CANNOT_EDIT_A_USER_WHO_IS_NOT_IN_THE_GROUP_WITH_ID,
                    jwtUser.getGroupId()
            ));
        }
    }

    @Deprecated
    // NotAllowedCreateMoreThanOneUserWithRoleAccountOwnerWithinAccountValidator
    private void checkAccountOwnerNotExist(User entity) {
        Map<String, List<ValidationError>> errors = new HashMap<>();

        if (entity.getRole().equals(roleService.getAccountOwnerRole())) {
            List<User> foundUsers = service.findAllByRoleAndAccountId(
                    roleService.getAccountOwnerRole(),
                    entity.getAccount().getId()
            );
            if (foundUsers.isEmpty()) {
                log.trace(USER_WITH_ROLE_ACCOUNT_OWNER_IS_UNIQUE, entity);
            } else {
                errors.put(USER_WITH_ROLE_ACCOUNT_OWNER, singletonList(new ValidationError(
                        NOT_UNIQUE_CODE,
                        format(NOT_UNIQUE_MESSAGE, USER_WITH_ROLE_ACCOUNT_OWNER)
                )));
            }
        }
        if (!errors.isEmpty()) {
            log.error(FIELDS_ARE_NOT_VALID, errors);
            throw new LogicalValidationException(VALIDATION_FAILED, errors);
        }
    }

    @Deprecated
    private void checkAccountOwnerExistsAfterUpdateAndRestrictionForEditEmailOfAccountOwner(User entity,
                                                                                            User userFromDatabase) {

        // Schema of constraints "Only one user must have a role "Account owner"
        // Entity from DB           | new  Entity               | result
        // has role Acc_Owner       | has role Acc_Owner        | not allowed (User with role ACCOUNT_OWNER is occupied)
        // has not role Acc_Owner   | has role Acc_Owner        | not possible
        // has role Acc_Owner       | has not role Acc_Owner    | all right
        // has not role Acc_Owner   | has not role Acc_Owner    | not allowed, Account must have user with role ACCOUNT_OWNER

        var errors = createMapForLogicalErrors();

        List<User> usersFromDatabaseWithRoleAccountOwner = service.findAllByRoleAndAccount_IdAndIdNot(
                roleService.getAccountOwnerRole(), entity.getAccount().getId(), entity.getId()
        );

        var oldUserHasRoleAccountOwner = !usersFromDatabaseWithRoleAccountOwner.isEmpty();
        var newUserHasRoleAccountOwner = entity.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString());

        // AfterUpdateCannotBeMoreThanOneUserWithRoleAccountOwnerWithinAccountValidator
        if (oldUserHasRoleAccountOwner && newUserHasRoleAccountOwner) {
            errors.put(USER_WITH_ROLE_ACCOUNT_OWNER, singletonList(new ValidationError(
                    NOT_UNIQUE_CODE,
                    format(NOT_UNIQUE_MESSAGE, USER_WITH_ROLE_ACCOUNT_OWNER)
            )));
        }

        // AfterUpdateMustBeOneUserWithRoleAccountOwnerWithinAccountValidator
        if (!oldUserHasRoleAccountOwner && !newUserHasRoleAccountOwner) {
            errors.put(USER_WITH_ROLE_ACCOUNT_OWNER, singletonList(new ValidationError(
                    NOT_UNIQUE_CODE,
                    ACCOUNT_MUST_HAVE_USER_WITH_ROLE_ACCOUNT_OWNER
            )));
        }
        // NotAllowedUpdateEmailOfAccountOwnerDirectlyValidator
        if (newUserHasRoleAccountOwner && !entity.getEmail().equals(userFromDatabase.getEmail())) {
            addValidationErrorIntoErrors(
                    USER_WITH_ROLE_ACCOUNT_OWNER,
                    YOU_CAN_EDIT_EMAIL_OF_THE_USER_WITH_A_ROLE_ACCOUNT_OWNER_ONLY_VIA_SPECIAL_PROCESS,
                    errors
            );
        }
        ifErrorsNotEmptyThrowLogicalValidationException(errors);
    }

    // NotAllowedChangeGroupOfUserIfUserWasUsedInTicketsValidator
    @Deprecated
    private void checkPossibilityOfChangingUserGroup(User entity) {
        var errors = createMapForLogicalErrors();
        var ticketServiceImpl =
                (RequestsFromModuleAccountAndUsers) appContext.getBean("ticketServiceImpl");
        long countTicket = ticketServiceImpl.countEntityWithUser(entity.getId());
        if (countTicket != 0) {
            addValidationErrorIntoErrors(
                    CHANGE_USER_GROUP,
                    THE_USER_CANNOT_CHANGE_THE_GROUP_BECAUSE_HE_IS_PRESENT_IN_TICKETS,
                    errors
            );
        }
        ifErrorsNotEmptyThrowLogicalValidationException(errors);
    }

}
