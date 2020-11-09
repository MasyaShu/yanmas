package ru.itterminal.botdesk.aau.service.validator;

import static java.lang.String.format;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static ru.itterminal.botdesk.aau.service.validator.UserOperationValidator.ACCOUNT_MUST_HAVE_USER_WITH_ROLE_ACCOUNT_OWNER;
import static ru.itterminal.botdesk.aau.service.validator.UserOperationValidator.CREATE_UPDATE_ONLY_HIS_GROUP;
import static ru.itterminal.botdesk.aau.service.validator.UserOperationValidator.INNER_GROUP;
import static ru.itterminal.botdesk.aau.service.validator.UserOperationValidator.USER_WITH_ROLE_ACCOUNT_OWNER;
import static ru.itterminal.botdesk.aau.service.validator.UserOperationValidator.WEIGHT_OF_ROLE;
import static ru.itterminal.botdesk.aau.service.validator.UserOperationValidator.WEIGHT_OF_ROLE_CURRENT_USER_LESS_THAN_WEIGHT_OF_ROLE_FROM_REQUEST;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.LOGIC_CONSTRAINT_CODE;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.NOT_UNIQUE_CODE;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.NOT_UNIQUE_MESSAGE;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.VALIDATION_FAILED;
import static ru.itterminal.botdesk.config.TestSecurityConfig.ACCOUNT_1_ID;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.projection.UserUniqueFields;
import ru.itterminal.botdesk.aau.service.impl.RoleServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.config.TestSecurityConfig;
import ru.itterminal.botdesk.jwt.JwtUser;

@SpringJUnitConfig(value = {UserOperationValidator.class})
@Import(TestSecurityConfig.class)
@ActiveProfiles("Test")
class UserOperationValidatorTest {

    @MockBean
    private UserServiceImpl service;

    @Mock
    private UserUniqueFields userUniqueFields;

    @MockBean
    private RoleServiceImpl roleService;

    final BCryptPasswordEncoder encoder = new BCryptPasswordEncoder();

    @Autowired
    private final UserOperationValidator validator = new UserOperationValidator(service, roleService);

    private static final String EXIST_EMAIL = "mail@mal.ru";
    private static final String NEW_USER_EMAIL = "mail_new@mal.ru";
    private static final String OLD_USER_EMAIL = "mail_old@mal.ru";
    private static final String ACCOUNT_NAME = "account name";
    private static User user;
    private static User oldUser;
    private static User newUser;
    private static LogicalValidationException logicalValidationException;
    private static final Map<String, List<ValidationError>> errors = new HashMap<>();

    @BeforeAll
    static void setUp() {
        Account account = Account.builder().name(ACCOUNT_NAME).build();
        account.setId(UUID.fromString(ACCOUNT_1_ID));
        Group group = Group.builder().build();
        String GROUP_ID = "dd06a563-802e-4d32-bd72-0555baa3e7e5";
        group.setId(UUID.fromString(GROUP_ID));
        user = User
                .builder()
                .email(EXIST_EMAIL)
                .account(account)
                .ownGroup(group)
                .build();
        oldUser = User
                .builder()
                .email(OLD_USER_EMAIL)
                .account(account)
                .ownGroup(group)
                .build();
        newUser = User
                .builder()
                .email(NEW_USER_EMAIL)
                .account(account)
                .ownGroup(group)
                .build();

    }

    @Test
    void checkUniqueness_shouldGetTrue_whenPassedDataIsUnique() {
        when(service.findByUniqueFields(any())).thenReturn(Collections.emptyList());
        assertTrue(validator.checkUniqueness(new User()));
    }

    @Test
    void checkUniqueness_shouldGetLogicalValidationException_whenPassedDataNotUnique() {
        when(service.findByUniqueFields(any())).thenReturn(List.of(userUniqueFields));
        when(userUniqueFields.getEmail()).thenReturn(EXIST_EMAIL);
        errors.put("email", singletonList(new ValidationError("not unique", "email is occupied")));
        logicalValidationException = new LogicalValidationException("Validation failed", errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.checkUniqueness(user));
        assertEquals(logicalValidationException.getFieldErrors().get("email").get(0),
                thrown.getFieldErrors().get("email").get(0));
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetLogicalValidationException_whenUserWithRoleAccountOwnerAlreadyExist() {
        user.setRole(Role
                .builder()
                .name(Roles.ACCOUNT_OWNER.toString())
                .weight(3)
                .build()
        );
        when(service.findAllByRoleAndAccountId(any(), any())).thenReturn(List.of(oldUser));
        when(roleService.getAccountOwnerRole())
                .thenReturn(Role
                        .builder()
                        .name(Roles.ACCOUNT_OWNER.toString())
                        .weight(3)
                        .build()
                );
        errors.put(USER_WITH_ROLE_ACCOUNT_OWNER, singletonList(new ValidationError(NOT_UNIQUE_CODE,
                format(NOT_UNIQUE_MESSAGE, USER_WITH_ROLE_ACCOUNT_OWNER))));
        logicalValidationException = new LogicalValidationException(VALIDATION_FAILED, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.beforeCreate(user));
        assertEquals(logicalValidationException.getFieldErrors().get(USER_WITH_ROLE_ACCOUNT_OWNER).get(0),
                thrown.getFieldErrors().get(USER_WITH_ROLE_ACCOUNT_OWNER).get(0));
    }

    @Test
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeCreate_shouldGetLogicalValidationException_whenUserIsNotInInnerGroupAndGroupIdsIsDifferent() {
        user.setRole(Role
                .builder()
                .name(Roles.AUTHOR.toString())
                .weight(0)
                .build()
        );
        when(service.findAllByRoleAndAccountId(any(), any())).thenReturn(List.of(oldUser));
        when(roleService.getAccountOwnerRole())
                .thenReturn(Role
                        .builder()
                        .name(Roles.ACCOUNT_OWNER.toString())
                        .weight(3)
                        .build()
                );
        errors.put(INNER_GROUP, singletonList(new ValidationError(LOGIC_CONSTRAINT_CODE,
                CREATE_UPDATE_ONLY_HIS_GROUP)));
        logicalValidationException = new LogicalValidationException(VALIDATION_FAILED, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.beforeCreate(user));
        assertEquals(logicalValidationException.getFieldErrors().get(INNER_GROUP).get(0),
                thrown.getFieldErrors().get(INNER_GROUP).get(0));
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetLogicalValidationException_whenWeightOfRoleOfCurrentUserLessThanWeightOfRoleCreatedEntity() {
        user.setRole(Role
                .builder()
                .name(Roles.ACCOUNT_OWNER.toString())
                .weight(3)
                .build()
        );
        when(service.findAllByRole(any())).thenReturn(List.of(oldUser));
        when(roleService.getAccountOwnerRole())
                .thenReturn(Role.builder().name(Roles.ACCOUNT_OWNER.toString()).build());
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        errors.put(WEIGHT_OF_ROLE, singletonList(new ValidationError(LOGIC_CONSTRAINT_CODE,
                format(WEIGHT_OF_ROLE_CURRENT_USER_LESS_THAN_WEIGHT_OF_ROLE_FROM_REQUEST, jwtUser, user))));
        logicalValidationException = new LogicalValidationException(VALIDATION_FAILED, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.beforeCreate(user));
        assertEquals(logicalValidationException.getFieldErrors().get(WEIGHT_OF_ROLE).get(0),
                thrown.getFieldErrors().get(WEIGHT_OF_ROLE).get(0));
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenNewAndOldUserWithRoleAccountOwner() {
        newUser.setRole(Role.builder().name(Roles.ACCOUNT_OWNER.toString()).build());
        oldUser.setRole(Role.builder().name(Roles.ACCOUNT_OWNER.toString()).build());
        when(service.findAllByRoleAndIdNot(any(), any())).thenReturn(List.of(oldUser));
        when(roleService.getAccountOwnerRole())
                .thenReturn(Role.builder().name(Roles.ACCOUNT_OWNER.toString()).build());
        errors.put(USER_WITH_ROLE_ACCOUNT_OWNER, singletonList(new ValidationError(NOT_UNIQUE_CODE,
                format(NOT_UNIQUE_MESSAGE, USER_WITH_ROLE_ACCOUNT_OWNER))));
        logicalValidationException = new LogicalValidationException(VALIDATION_FAILED, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.beforeUpdate(newUser));
        assertEquals(logicalValidationException.getFieldErrors().get(USER_WITH_ROLE_ACCOUNT_OWNER).get(0),
                thrown.getFieldErrors().get(USER_WITH_ROLE_ACCOUNT_OWNER).get(0));
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenOldAndNewUserWithoutRoleAccountOwner() {
        newUser.setRole(Role.builder().name(Roles.AUTHOR.toString()).build());
        when(service.findAllByRoleAndIdNot(any(), any())).thenReturn(Collections.emptyList());
        when(roleService.getAccountOwnerRole())
                .thenReturn(Role.builder().name(Roles.ACCOUNT_OWNER.toString()).build());
        errors.put(USER_WITH_ROLE_ACCOUNT_OWNER, singletonList(new ValidationError(NOT_UNIQUE_CODE,
                ACCOUNT_MUST_HAVE_USER_WITH_ROLE_ACCOUNT_OWNER)));
        logicalValidationException = new LogicalValidationException(VALIDATION_FAILED, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.beforeUpdate(newUser));
        assertEquals(logicalValidationException.getFieldErrors().get(USER_WITH_ROLE_ACCOUNT_OWNER).get(0),
                thrown.getFieldErrors().get(USER_WITH_ROLE_ACCOUNT_OWNER).get(0));
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenWeightOfRoleOfCurrentUserLessThanWeightOfRoleUpdatedEntity() {
        user.setRole(
                Role
                        .builder()
                        .name(Roles.ACCOUNT_OWNER.toString())
                        .weight(3)
                        .build()
        );
        when(service.findAllByRole(any())).thenReturn(List.of(oldUser));
        when(roleService.getAccountOwnerRole())
                .thenReturn(Role.builder().name(Roles.ACCOUNT_OWNER.toString()).build());
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        errors.put(WEIGHT_OF_ROLE, singletonList(new ValidationError(LOGIC_CONSTRAINT_CODE,
                format(WEIGHT_OF_ROLE_CURRENT_USER_LESS_THAN_WEIGHT_OF_ROLE_FROM_REQUEST, jwtUser, user))));
        logicalValidationException = new LogicalValidationException(VALIDATION_FAILED, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.beforeUpdate(user));
        assertEquals(logicalValidationException.getFieldErrors().get(WEIGHT_OF_ROLE).get(0),
                thrown.getFieldErrors().get(WEIGHT_OF_ROLE).get(0));
    }

    @Test
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenUserIsNotInInnerGroupAndGroupIdsIsDifferent() {
        user.setRole(Role
                .builder()
                .name(Roles.AUTHOR.toString())
                .weight(0)
                .build()
        );
        when(service.findAllByRoleAndAccountId(any(), any())).thenReturn(List.of(oldUser));
        when(roleService.getAccountOwnerRole())
                .thenReturn(Role
                        .builder()
                        .name(Roles.ACCOUNT_OWNER.toString())
                        .weight(3)
                        .build()
                );
        errors.put(INNER_GROUP, singletonList(new ValidationError(LOGIC_CONSTRAINT_CODE,
                CREATE_UPDATE_ONLY_HIS_GROUP)));
        logicalValidationException = new LogicalValidationException(VALIDATION_FAILED, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.beforeUpdate(user));
        assertEquals(logicalValidationException.getFieldErrors().get(INNER_GROUP).get(0),
                thrown.getFieldErrors().get(INNER_GROUP).get(0));
    }

    @Test
    void encoderPassword() {
        String encodedPassword = encoder.encode("12345");
        System.out.println("encodedPassword: " + encodedPassword);
        assertTrue(encoder.matches("12345", encodedPassword));
    }

}