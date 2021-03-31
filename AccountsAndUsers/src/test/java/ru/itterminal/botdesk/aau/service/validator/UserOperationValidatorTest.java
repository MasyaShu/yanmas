package ru.itterminal.botdesk.aau.service.validator;

import static java.lang.String.format;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static ru.itterminal.botdesk.aau.service.validator.UserOperationValidator.*;
import static ru.itterminal.botdesk.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;
import static ru.itterminal.botdesk.security.config.TestSecurityConfig.INNER_GROUP_ID;
import static ru.itterminal.botdesk.security.config.TestSecurityConfig.NOT_INNER_GROUP_ID;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Import;
import org.springframework.security.access.AccessDeniedException;
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
import ru.itterminal.botdesk.aau.model.test.RoleTestHelper;
import ru.itterminal.botdesk.aau.service.impl.RoleServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.integration.across_modules.RequestsFromModuleAccountAndUsers;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {UserOperationValidator.class})
@Import(TestSecurityConfig.class)
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class UserOperationValidatorTest {

    @MockBean
    private UserServiceImpl service;

    @Mock
    private UserUniqueFields userUniqueFields;

    @Mock
    private ApplicationContext appContext;

    @MockBean
    private RoleServiceImpl roleService;

    @MockBean(name = "ticketServiceImpl")
    private RequestsFromModuleAccountAndUsers ticketServiceImpl;

    final BCryptPasswordEncoder encoder = new BCryptPasswordEncoder();

    @Autowired
    private final UserOperationValidator validator = new UserOperationValidator(service, roleService, appContext);

    private static final String EXIST_EMAIL = "mail@mal.ru";
    private static final String NEW_USER_EMAIL = "mail_new@mal.ru";
    private static final String OLD_USER_EMAIL = "mail_old@mal.ru";
    private static final String ACCOUNT_NAME = "account name";
    private User user;
    private User oldUser;
    private User userFromDatabase;
    private User newUser;
    private LogicalValidationException logicalValidationException;
    private final Map<String, List<ValidationError>> errors = new HashMap<>();
    private final RoleTestHelper roleTestHelper = new RoleTestHelper();
    private final Role roleAccountOwner = roleTestHelper.getRoleByName(Roles.ACCOUNT_OWNER.toString());
    private final Role roleAdmin = roleTestHelper.getRoleByName(Roles.ADMIN.toString());
    private final Role roleExecutor = roleTestHelper.getRoleByName(Roles.EXECUTOR.toString());
    private final Role roleAuthor = roleTestHelper.getRoleByName(Roles.AUTHOR.toString());
    private final Role roleObserver = roleTestHelper.getRoleByName(Roles.OBSERVER.toString());

    @BeforeEach
    void setUp() {
        Account account = Account.builder().name(ACCOUNT_NAME).build();
        account.setId(UUID.fromString(TestSecurityConfig.ACCOUNT_1_ID));
        Group group = Group.builder().build();
        String GROUP_ID = "dd06a563-802e-4d32-bd72-0555baa3e7e5";
        group.setId(UUID.fromString(GROUP_ID));
        Group groupUserFormDatabase = Group.builder().build();
        groupUserFormDatabase.setId(UUID.randomUUID());
        user = User
                .builder()
                .email(EXIST_EMAIL)
                .account(account)
                .group(group)
                .role(roleAdmin)
                .build();
        oldUser = User
                .builder()
                .email(OLD_USER_EMAIL)
                .account(account)
                .group(group)
                .build();
        userFromDatabase = User
                .builder()
                .email(OLD_USER_EMAIL)
                .account(account)
                .role(roleAccountOwner)
                .group(group)
                .build();
        newUser = User
                .builder()
                .email(NEW_USER_EMAIL)
                .account(account)
                .group(group)
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
        LogicalValidationException thrown = assertThrows(
                LogicalValidationException.class,
                () -> validator.checkUniqueness(user)
        );
        assertEquals(
                logicalValidationException.getFieldErrors().get("email").get(0),
                thrown.getFieldErrors().get("email").get(0)
        );
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void checkAccessForUpdate_shouldGetAccessDeniedException_whenCurrentUserRoleIsLessThanUpdatedUserRole() {
        newUser.setRole(roleAuthor);
        userFromDatabase.setRole(roleExecutor);
        when(service.findById(newUser.getId())).thenReturn(userFromDatabase);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        AccessDeniedException thrown = assertThrows(
                AccessDeniedException.class,
                () -> validator.checkAccessBeforeUpdate(newUser)
        );
        assertEquals(
                format(
                        IS_FORBIDDEN_TO_CHANGE_THE_USER_WITH_THE_ROLE,
                        newUser.getRole().getDisplayName()
                ),
                thrown.getMessage()
        );
        verify(service, times(1)).findById(newUser.getId());
    }

    @Test
    void encoderPassword() {
        String encodedPassword = encoder.encode("12345");
        System.out.println("encodedPassword: " + encodedPassword);
        assertTrue(encoder.matches("12345", encodedPassword));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void checkAccessForRead_shouldGetAccessDeniedException_whenCurrentUserIsFromNotInnerGroupAndCurrentUserGroupNotEqualsEntityGroup() {
        user.getGroup().setId(UUID.fromString(INNER_GROUP_ID));
        AccessDeniedException thrown = assertThrows(
                AccessDeniedException.class,
                () -> validator.checkAccessBeforeRead(user)
        );
        assertEquals(ACCESS_IS_DENIED_FOR_SEARCHING_BY_PASSED_ID, thrown.getMessage());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void checkAccessForRead_shouldNotGetException_whenCurrentUserFromNotInnerGroupAndCurrentUserGroupEqualsEntityGroup() {
        user.getGroup().setId(UUID.fromString(NOT_INNER_GROUP_ID));
        validator.checkAccessBeforeRead(user);
    }

    @ParameterizedTest
    @MethodSource("getUserRolesThatAnAccountOwnerCanCreate")
    @WithUserDetails("OWNER_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetTrue_whenAccountOwnerRoleAllowsYouToCreateUsersWithPassedRoles(Role role) {
        user.setRole(role);
        when(roleService.getAccountOwnerRole())
                .thenReturn(roleAccountOwner);
        assertTrue(validator.logicalValidationBeforeCreate(user));
        verify(service, times(0)).findAllByRoleAndAccountId(any(), any());
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetLogicalValidationException_WhenUserWithRoleAccountOwnerExistsInDatabase() {
        user.setRole(roleAccountOwner);
        when(service.findAllByRoleAndAccountId(any(), any())).thenReturn(List.of(oldUser));
        when(roleService.getAccountOwnerRole())
                .thenReturn(roleAccountOwner);
        errors.put(USER_WITH_ROLE_ACCOUNT_OWNER, singletonList(new ValidationError(
                NOT_UNIQUE_CODE,
                format(NOT_UNIQUE_MESSAGE, USER_WITH_ROLE_ACCOUNT_OWNER)
        )));
        logicalValidationException = new LogicalValidationException(VALIDATION_FAILED, errors);
        LogicalValidationException thrown = assertThrows(
                LogicalValidationException.class,
                () -> validator.logicalValidationBeforeCreate(user)
        );
        assertEquals(
                logicalValidationException.getFieldErrors().get(USER_WITH_ROLE_ACCOUNT_OWNER).get(0),
                thrown.getFieldErrors().get(USER_WITH_ROLE_ACCOUNT_OWNER).get(0)
        );
    }

    @ParameterizedTest
    @MethodSource("getUserRolesThatAnAccountOwnerCanUpdate")
    @WithUserDetails("OWNER_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetTrue_whenAccountOwnerRoleAllowsYouToUpdateUsersWithPassedRoles(Role role) {
        if (role.getName().equals(Roles.ACCOUNT_OWNER.toString())) {
            newUser.setRole(role);
            newUser.setEmail(oldUser.getEmail());
        } else {
            newUser.setRole(role);
        }
        oldUser.setRole(role);
        when(service.findAllByRoleAndAccount_IdAndIdNot(any(), any(), any()))
                .thenReturn((role.equals(roleAccountOwner)) ? List.of() : List.of(oldUser));
        when(service.findById(newUser.getId())).thenReturn(userFromDatabase);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        assertTrue(validator.logicalValidationBeforeUpdate(newUser));
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenUpdateEmailOfAccountOwner() {
        newUser.setRole(roleAccountOwner);
        oldUser.setRole(roleAdmin);
        when(service.findAllByRoleAndAccount_IdAndIdNot(any(), any(), any()))
                .thenReturn(List.of());
        when(service.findById(newUser.getId())).thenReturn(userFromDatabase);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        var logicalValidationException
                = assertThrows(LogicalValidationException.class, () -> validator.logicalValidationBeforeUpdate(newUser));
        assertEquals(
                YOU_CAN_EDIT_EMAIL_OF_THE_USER_WITH_A_ROLE_ACCOUNT_OWNER_ONLY_VIA_SPECIAL_PROCESS,
                logicalValidationException.getFieldErrors().get(USER_WITH_ROLE_ACCOUNT_OWNER).get(0).getMessage()
        );
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException2_whenUserHasAGroupChangeAndIsAlreadyInTheTickets() {
        newUser.setRole(roleAccountOwner);
        userFromDatabase.setGroup(Group.builder().id(UUID.randomUUID()).build());
        when(service.findAllByRoleAndAccount_IdAndIdNot(any(), any(), any()))
                .thenReturn(List.of());
        when(service.findById(newUser.getId())).thenReturn(userFromDatabase);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        when(ticketServiceImpl.countEntityWithUser(any())).thenReturn(2L);
        var logicalValidationException
                = assertThrows(LogicalValidationException.class, () -> validator.logicalValidationBeforeUpdate(newUser));
        assertEquals(
                THE_USER_CANNOT_CHANGE_THE_GROUP_BECAUSE_HE_IS_PRESENT_IN_TICKETS,
                logicalValidationException.getFieldErrors().get(CHANGE_USER_GROUP).get(0).getMessage()
        );
    }

    @ParameterizedTest
    @MethodSource("getUserRolesThatAnAdminInnerGroupCanCreateAndUpdate")
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetTrue_whenCurrentUserFromInnerGroupAndAdminRoleAllowsYouToCreateUsersWithPassedRoles(
            Role role, String idGroup) {
        user.setRole(role);
        user.getGroup().setId(UUID.fromString(idGroup));
        when(roleService.getAccountOwnerRole())
                .thenReturn(roleAccountOwner);
        assertTrue(validator.logicalValidationBeforeCreate(user));
        verify(service, times(0)).findAllByRoleAndAccountId(any(), any());
    }

    @ParameterizedTest
    @MethodSource("getUserRolesThatAnAdminInnerGroupCanCreateAndUpdate")
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetTrue_whenCurrentUserFromInnerGroupAndAdminRoleAllowsYouToUpdateUsersWithPassedRoles(
            Role role, String idGroup) {
        newUser.setRole(role);
        user.getGroup().setId(UUID.fromString(idGroup));
        userFromDatabase.setRole(role);
        userFromDatabase.getGroup().setId(UUID.fromString(idGroup));
        when(service.findAllByRoleAndAccount_IdAndIdNot(any(), any(), any())).thenReturn(List.of(oldUser));
        when(service.findById(newUser.getId())).thenReturn(userFromDatabase);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        assertTrue(validator.logicalValidationBeforeUpdate(newUser));
    }

    @ParameterizedTest
    @MethodSource("getUserRolesThatAnAdminNotInnerGroupCanCreateAndUpdate")
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeCreate_shouldGetTrue_whenCurrentUserFromNotInnerGroupAndAdminRoleAllowsYouToCreateUsersWithPassedRoles(
            Role role, String idGroup) {
        user.setRole(role);
        user.getGroup().setId(UUID.fromString(idGroup));
        when(roleService.getAccountOwnerRole())
                .thenReturn(roleAccountOwner);
        assertTrue(validator.logicalValidationBeforeCreate(user));
        verify(service, times(0)).findAllByRoleAndAccountId(any(), any());
    }

    @ParameterizedTest
    @MethodSource("getUserRolesThatAnAdminNotInnerGroupCanCreateAndUpdate")
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeUpdate_shouldGetTrue_whenCurrentUserFromNotInnerGroupAndAdminRoleAllowsYouToUpdateUsersWithPassedRoles(
            Role role, String idGroup) {
        newUser.setRole(role);
        user.getGroup().setId(UUID.fromString(idGroup));
        userFromDatabase.setRole(role);
        userFromDatabase.getGroup().setId(UUID.fromString(idGroup));
        when(service.findAllByRoleAndAccount_IdAndIdNot(any(), any(), any())).thenReturn(List.of(oldUser));
        when(service.findById(newUser.getId())).thenReturn(userFromDatabase);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        assertTrue(validator.logicalValidationBeforeUpdate(newUser));
    }

    @ParameterizedTest
    @MethodSource("getUserRolesThatAnExecutorInnerGroupNotCanCreateAndUpdate")
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_INNER_GROUP")
    void checkAccessForCreate_shouldLogicalValidationException_whenCurrentUserFromInnerGroupAndExecutorRoleNotAllowsYouToCreateUsersWithPassedRoles(
            Role role, String idGroup) {
        user.setRole(role);
        user.getGroup().setId(UUID.fromString(idGroup));
        when(roleService.getAccountOwnerRole())
                .thenReturn(roleAccountOwner);
        assertThrows(
                AccessDeniedException.class,
                () -> validator.checkAccessBeforeCreate(user)
        );
    }

    @ParameterizedTest
    @MethodSource("getUserRolesThatAnExecutorInnerGroupNotCanCreateAndUpdate")
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_INNER_GROUP")
    void checkAccessForUpdate_shouldLogicalValidationException_whenCurrentUserFromInnerGroupAndExecutorRoleNotAllowsYouToUpdateUsersWithPassedRoles(
            Role role, String idGroup) {
        newUser.setRole(role);
        user.getGroup().setId(UUID.fromString(idGroup));
        userFromDatabase.setRole(role);
        userFromDatabase.getGroup().setId(UUID.fromString(idGroup));
        when(service.findAllByRoleAndAccount_IdAndIdNot(any(), any(), any())).thenReturn(List.of(oldUser));
        when(service.findById(newUser.getId())).thenReturn(userFromDatabase);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        assertThrows(
                AccessDeniedException.class,
                () -> validator.checkAccessBeforeUpdate(newUser)
        );
    }

    @ParameterizedTest
    @MethodSource("getUserRolesThatAnExecutorNotInnerGroupNotCanCreateAndUpdate")
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void checkAccessForCreate_shouldGetLogicalValidationException_whenCurrentUserFromNotInnerGroupAndExecutorRoleNotAllowsYouToCreateUsersWithPassedRoles(
            Role role, String idGroup) {
        user.setRole(role);
        user.getGroup().setId(UUID.fromString(idGroup));
        when(roleService.getAccountOwnerRole())
                .thenReturn(roleAccountOwner);
        assertThrows(
                AccessDeniedException.class,
                () -> validator.checkAccessBeforeCreate(user)
        );
    }

    @ParameterizedTest
    @MethodSource("getUserRolesThatAnExecutorNotInnerGroupNotCanCreateAndUpdate")
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void checkAccessForUpdate_shouldLogicalValidationException_whenCurrentUserFromNotInnerGroupAndExecutorRoleNotAllowsYouToUpdateUsersWithPassedRoles(
            Role role, String idGroup) {
        newUser.setRole(role);
        user.getGroup().setId(UUID.fromString(idGroup));
        userFromDatabase.setRole(role);
        userFromDatabase.getGroup().setId(UUID.fromString(idGroup));
        when(service.findAllByRoleAndAccount_IdAndIdNot(any(), any(), any())).thenReturn(List.of(oldUser));
        when(service.findById(newUser.getId())).thenReturn(userFromDatabase);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        assertThrows(
                AccessDeniedException.class,
                () -> validator.checkAccessBeforeUpdate(newUser)
        );
    }

    private Stream<Arguments> getUserRolesThatAnAccountOwnerCanCreate() {
        return Stream.of(
                Arguments.of(roleAdmin),
                Arguments.of(roleExecutor),
                Arguments.of(roleAuthor),
                Arguments.of(roleObserver)
        );
    }

    private Stream<Arguments> getUserRolesThatAnAccountOwnerCanUpdate() {
        return Stream.of(
                Arguments.of(roleAccountOwner),
                Arguments.of(roleAdmin),
                Arguments.of(roleExecutor),
                Arguments.of(roleAuthor),
                Arguments.of(roleObserver)
        );
    }

    private Stream<Arguments> getUserRolesThatAnAdminInnerGroupCanCreateAndUpdate() {
        return Stream.of(
                Arguments.of(roleAdmin, INNER_GROUP_ID),
                Arguments.of(roleAdmin, NOT_INNER_GROUP_ID),
                Arguments.of(roleExecutor, INNER_GROUP_ID),
                Arguments.of(roleExecutor, NOT_INNER_GROUP_ID),
                Arguments.of(roleAuthor, INNER_GROUP_ID),
                Arguments.of(roleAuthor, NOT_INNER_GROUP_ID),
                Arguments.of(roleObserver, INNER_GROUP_ID),
                Arguments.of(roleObserver, NOT_INNER_GROUP_ID)
        );
    }

    private Stream<Arguments> getUserRolesThatAnAdminNotInnerGroupCanCreateAndUpdate() {
        return Stream.of(
                Arguments.of(roleAdmin, NOT_INNER_GROUP_ID),
                Arguments.of(roleExecutor, NOT_INNER_GROUP_ID),
                Arguments.of(roleAuthor, NOT_INNER_GROUP_ID),
                Arguments.of(roleObserver, NOT_INNER_GROUP_ID)
        );
    }

    private Stream<Arguments> getUserRolesThatAnExecutorInnerGroupNotCanCreateAndUpdate() {
        return Stream.of(
                Arguments.of(roleAccountOwner, INNER_GROUP_ID),
                Arguments.of(roleAccountOwner, NOT_INNER_GROUP_ID),
                Arguments.of(roleAdmin, INNER_GROUP_ID),
                Arguments.of(roleAdmin, NOT_INNER_GROUP_ID)
        );
    }

    private Stream<Arguments> getUserRolesThatAnExecutorNotInnerGroupNotCanCreateAndUpdate() {
        return Stream.of(
                Arguments.of(roleAccountOwner, INNER_GROUP_ID),
                Arguments.of(roleAccountOwner, NOT_INNER_GROUP_ID),
                Arguments.of(roleAdmin, INNER_GROUP_ID),
                Arguments.of(roleAdmin, NOT_INNER_GROUP_ID),
                Arguments.of(roleExecutor, INNER_GROUP_ID),
                Arguments.of(roleAuthor, INNER_GROUP_ID),
                Arguments.of(roleObserver, INNER_GROUP_ID)
        );
    }

}
