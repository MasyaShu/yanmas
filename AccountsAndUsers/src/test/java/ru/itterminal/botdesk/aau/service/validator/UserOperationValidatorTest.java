package ru.itterminal.botdesk.aau.service.validator;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.botdesk.aau.model.*;
import ru.itterminal.botdesk.aau.model.projection.UserUniqueFields;
import ru.itterminal.botdesk.aau.model.test.RoleTestHelper;
import ru.itterminal.botdesk.aau.service.impl.RoleServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.security.jwt.JwtUser;

import java.util.*;
import java.util.stream.Stream;

import static java.lang.String.format;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static ru.itterminal.botdesk.aau.service.validator.UserOperationValidator.*;
import static ru.itterminal.botdesk.security.config.TestSecurityConfig.GROUP_1_ID;
import static ru.itterminal.botdesk.security.config.TestSecurityConfig.GROUP_2_ID;

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
    private static User userFromDatabase;
    private static User newUser;
    private static Role roleAccountOwner;
    private static Role roleAdmin;
    private static Role roleExecutor;
    private static Role roleAuthor;
    private static Role roleObserver;
    private static LogicalValidationException logicalValidationException;
    private static final Map<String, List<ValidationError>> errors = new HashMap<>();
    private static final RoleTestHelper roleTestHelper = new RoleTestHelper();

    @BeforeAll
    static void setUp() {
        Account account = Account.builder().name(ACCOUNT_NAME).build();
        account.setId(UUID.fromString(TestSecurityConfig.ACCOUNT_1_ID));
        Group group = Group.builder().build();
        String GROUP_ID = "dd06a563-802e-4d32-bd72-0555baa3e7e5";
        group.setId(UUID.fromString(GROUP_ID));
        Group groupUserFormDatabase = Group.builder().build();
        groupUserFormDatabase.setId(UUID.randomUUID());
        List<Role> roleList = roleTestHelper.setPredefinedValidEntityList();
        roleAccountOwner = roleList.get(0);
        roleAdmin = roleList.get(1);
        roleExecutor = roleList.get(2);
        roleAuthor = roleList.get(3);
        roleObserver = roleList.get(4);
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
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.checkUniqueness(user));
        assertEquals(logicalValidationException.getFieldErrors().get("email").get(0),
                thrown.getFieldErrors().get("email").get(0));
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetLogicalValidationException_whenUserWithRoleAccountOwnerAlreadyExist() {
        user.setRole(roleAccountOwner);
        when(service.findAllByRoleAndAccountId(any(), any())).thenReturn(List.of(oldUser));
        when(roleService.getAccountOwnerRole())
                .thenReturn(roleAccountOwner);
        errors.put(USER_WITH_ROLE_ACCOUNT_OWNER, singletonList(new ValidationError(NOT_UNIQUE_CODE,
                format(NOT_UNIQUE_MESSAGE, USER_WITH_ROLE_ACCOUNT_OWNER))));
        logicalValidationException = new LogicalValidationException(VALIDATION_FAILED, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.beforeCreate(user));
        assertEquals(logicalValidationException.getFieldErrors().get(USER_WITH_ROLE_ACCOUNT_OWNER).get(0),
                thrown.getFieldErrors().get(USER_WITH_ROLE_ACCOUNT_OWNER).get(0));
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldExceptionUserWithRoleIsOccupied_whenCreateAnotherAccountOwner() {
        user.setRole(roleAccountOwner);
        when(service.findAllByRoleAndAccountId(any(), any())).thenReturn(List.of(oldUser));
        when(roleService.getAccountOwnerRole())
                .thenReturn(roleAccountOwner);
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
        user.setRole(roleAuthor);
        when(service.findAllByRoleAndAccountId(any(), any())).thenReturn(List.of(oldUser));
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
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
        user.setRole(roleAccountOwner);
        when(service.findAllByRole(any())).thenReturn(List.of(oldUser));
        when(roleService.getAccountOwnerRole())
                .thenReturn(Role.builder().name(Roles.ACCOUNT_OWNER.toString()).build());
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        errors.put(WEIGHT_OF_ROLE, singletonList(new ValidationError(LOGIC_CONSTRAINT_CODE,
                format(WEIGHT_OF_ROLE_CURRENT_USER_LESS_THAN_WEIGHT_OF_ROLE_FROM_REQUEST, jwtUser.getWeightRole(),
                        user.getRole().getWeight()))));
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
        when(service.findAllByRoleAndAccount_IdAndIdNot(any(), any(), any())).thenReturn(List.of(oldUser));
        when(service.findById(newUser.getId())).thenReturn(userFromDatabase);
        when(roleService.getAccountOwnerRole())
                .thenReturn(Role.builder().name(Roles.ACCOUNT_OWNER.toString()).build());
        errors.put(USER_WITH_ROLE_ACCOUNT_OWNER, singletonList(new ValidationError(NOT_UNIQUE_CODE,
                format(NOT_UNIQUE_MESSAGE, USER_WITH_ROLE_ACCOUNT_OWNER))));
        logicalValidationException = new LogicalValidationException(VALIDATION_FAILED, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.beforeUpdate(newUser));
        assertEquals(logicalValidationException.getFieldErrors().get(USER_WITH_ROLE_ACCOUNT_OWNER).get(0),
                thrown.getFieldErrors().get(USER_WITH_ROLE_ACCOUNT_OWNER).get(0));
        verify(service, times(1)).findAllByRoleAndAccount_IdAndIdNot(any(), any(), any());
        verify(service, times(1)).findById(newUser.getId());
        verify(roleService, times(1)).getAccountOwnerRole();
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenOldAndNewUserWithoutRoleAccountOwner() {
        newUser.setRole(Role.builder().name(Roles.AUTHOR.toString()).build());
        when(service.findAllByRoleAndIdNot(any(), any())).thenReturn(Collections.emptyList());
        when(service.findById(newUser.getId())).thenReturn(userFromDatabase);
        when(roleService.getAccountOwnerRole())
                .thenReturn(Role.builder().name(Roles.ACCOUNT_OWNER.toString()).build());
        errors.put(USER_WITH_ROLE_ACCOUNT_OWNER, singletonList(new ValidationError(NOT_UNIQUE_CODE,
                ACCOUNT_MUST_HAVE_USER_WITH_ROLE_ACCOUNT_OWNER)));
        logicalValidationException = new LogicalValidationException(VALIDATION_FAILED, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.beforeUpdate(newUser));
        assertEquals(logicalValidationException.getFieldErrors().get(USER_WITH_ROLE_ACCOUNT_OWNER).get(0),
                thrown.getFieldErrors().get(USER_WITH_ROLE_ACCOUNT_OWNER).get(0));
        verify(service, times(1)).findAllByRoleAndAccount_IdAndIdNot(any(), any(), any());
        verify(service, times(1)).findById(newUser.getId());
        verify(roleService, times(1)).getAccountOwnerRole();
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenWeightOfRoleOfCurrentUserLessThanWeightOfRoleUpdatingUser() {
        user.setRole(
                Role
                        .builder()
                        .name(Roles.ACCOUNT_OWNER.toString())
                        .weight(Roles.ACCOUNT_OWNER.getWeight())
                        .build()
        );
        when(service.findAllByRole(any())).thenReturn(List.of(oldUser));
        when(service.findById(user.getId())).thenReturn(userFromDatabase);
        when(roleService.getAccountOwnerRole())
                .thenReturn(Role.builder().name(Roles.ACCOUNT_OWNER.toString()).build());
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        errors.put(WEIGHT_OF_ROLE, singletonList(new ValidationError(LOGIC_CONSTRAINT_CODE,
                format(WEIGHT_OF_ROLE_CURRENT_USER_LESS_THAN_WEIGHT_OF_ROLE_FROM_REQUEST, jwtUser.getWeightRole(),
                        user.getRole().getWeight()))));
        logicalValidationException = new LogicalValidationException(VALIDATION_FAILED, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.beforeUpdate(user));
        assertEquals(logicalValidationException.getFieldErrors().get(WEIGHT_OF_ROLE).get(0),
                thrown.getFieldErrors().get(WEIGHT_OF_ROLE).get(0));
        verify(service, times(1)).findAllByRoleAndAccount_IdAndIdNot(any(), any(), any());
        verify(service, times(1)).findById(user.getId());
        verify(roleService, times(1)).getAccountOwnerRole();
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenWeightOfRoleOfCurrentUserLessThanWeightOfRoleUpdatingUserFromDatabase() {
        user.setRole(
                Role
                        .builder()
                        .name(Roles.ACCOUNT_OWNER.toString())
                        .weight(Roles.ACCOUNT_OWNER.getWeight())
                        .build()
        );
        when(service.findAllByRole(any())).thenReturn(List.of(oldUser));
        when(service.findById(any())).thenReturn(userFromDatabase);
        when(roleService.getAccountOwnerRole())
                .thenReturn(Role.builder().name(Roles.ACCOUNT_OWNER.toString()).build());
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        errors.put(WEIGHT_OF_ROLE_USER_FROM_DATABASE, singletonList(new ValidationError(LOGIC_CONSTRAINT_CODE,
                format(WEIGHT_OF_ROLE_CURRENT_USER_LESS_THAN_WEIGHT_OF_ROLE_FROM_REQUEST, jwtUser.getWeightRole(),
                        user.getRole().getWeight()))));
        logicalValidationException = new LogicalValidationException(VALIDATION_FAILED, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.beforeUpdate(user));
        assertEquals(logicalValidationException.getFieldErrors().get(WEIGHT_OF_ROLE_USER_FROM_DATABASE).get(0),
                thrown.getFieldErrors().get(WEIGHT_OF_ROLE).get(0));
        verify(service, times(1)).findAllByRoleAndAccount_IdAndIdNot(any(), any(), any());
        verify(service, times(1)).findById(user.getId());
        verify(roleService, times(1)).getAccountOwnerRole();
    }

    @Test
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenUserIsNotInInnerGroupAndGroupOfCurrentUserIsNotEqualGroupOfUpdatingUser() {
        user.setRole(roleAuthor);
        user.getGroup().setId(UUID.fromString(GROUP_1_ID));
        when(service.findAllByRoleAndAccount_IdAndIdNot(any(), any(), any())).thenReturn(List.of(oldUser));
        when(service.findById(any())).thenReturn(userFromDatabase);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        errors.put(INNER_GROUP, singletonList(new ValidationError(LOGIC_CONSTRAINT_CODE,
                CREATE_UPDATE_ONLY_HIS_GROUP)));
        logicalValidationException = new LogicalValidationException(VALIDATION_FAILED, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.beforeUpdate(user));
        assertEquals(logicalValidationException.getFieldErrors().get(INNER_GROUP).get(0),
                thrown.getFieldErrors().get(INNER_GROUP).get(0));
        verify(service, times(1)).findAllByRoleAndAccount_IdAndIdNot(any(), any(), any());
        verify(service, times(1)).findAllByRoleAndAccount_IdAndIdNot(any(), any(), any());
        verify(service, times(1)).findById(user.getId());
        verify(roleService, times(1)).getAccountOwnerRole();

    }

    @Test
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenUserIsNotInInnerGroupAndGroupOfCurrentUserIsNotEqualGroupOfUserFromDatabase() {
        user.setRole(roleAuthor);
        user.getGroup().setId(UUID.fromString(GROUP_1_ID));
        when(service.findAllByRoleAndAccount_IdAndIdNot(any(), any(), any())).thenReturn(List.of(oldUser));
        when(service.findById(any())).thenReturn(userFromDatabase);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        errors.put(INNER_GROUP_USER_FROM_DATABASE, singletonList(new ValidationError(LOGIC_CONSTRAINT_CODE,
                CREATE_UPDATE_ONLY_HIS_GROUP)));
        logicalValidationException = new LogicalValidationException(VALIDATION_FAILED, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.beforeUpdate(user));
        assertEquals(logicalValidationException.getFieldErrors().get(INNER_GROUP_USER_FROM_DATABASE).get(0),
                thrown.getFieldErrors().get(INNER_GROUP_USER_FROM_DATABASE).get(0));
        verify(service, times(1)).findAllByRoleAndAccount_IdAndIdNot(any(), any(), any());
        verify(service, times(1)).findById(user.getId());
        verify(roleService, times(1)).getAccountOwnerRole();

    }

    @Test
    void encoderPassword() {
        String encodedPassword = encoder.encode("12345");
        System.out.println("encodedPassword: " + encodedPassword);
        assertTrue(encoder.matches("12345", encodedPassword));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void checkAccessForRead_shouldGetAccessError_whenUserNotInnerGroupAndGroupUserNoEqualsEntity() {
        user.getGroup().setId(UUID.randomUUID());
        assertThrows(AccessDeniedException.class,
                () -> validator.checkAccessForRead(user));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void checkAccessForRead_shouldGetTrue_whenUserNotInnerGroupAndGroupUserEqualsEntity() {
        user.getGroup().setId(UUID.fromString(GROUP_2_ID));
        assertTrue(validator.checkAccessForRead(user));
    }

    @ParameterizedTest
    @MethodSource("getUserRolesThatAnAccountOwnerCanCreate")
    @WithUserDetails("OWNER_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldCreateUser_whenRoleAccountOwnerWeightCreatedUserLess(Role role) {
        user.setRole(role);
        when(roleService.getAccountOwnerRole())
                .thenReturn(roleAccountOwner);
        assertTrue(validator.beforeCreate(user));
        verify(service, times(0)).findAllByRoleAndAccountId(any(), any());
    }

    @ParameterizedTest
    @MethodSource("getUserRolesThatAnAccountOwnerCanUpdate")
    @WithUserDetails("OWNER_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldUpdateUser_whenRoleAccountOwnerWeightCreatedUserEqualsLess(Role role) {
        newUser.setRole(role);
        oldUser.setRole(role);
        when(service.findAllByRoleAndAccount_IdAndIdNot(any(), any(), any())).thenReturn((role.equals(roleAccountOwner))? List.of():List.of(oldUser));
        when(service.findById(newUser.getId())).thenReturn(userFromDatabase);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        assertTrue(validator.beforeUpdate(newUser));
    }

    @ParameterizedTest
    @MethodSource("getUserRolesThatAnAdminInnerGroupCanCreateAndUpdate")
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldCreateUser_whenRoleAdminWeightCreatedUserLess(Role role, String idGroup) {
        user.setRole(role);
        user.getGroup().setId(UUID.fromString(idGroup));
        when(roleService.getAccountOwnerRole())
                .thenReturn(roleAccountOwner);
        assertTrue(validator.beforeCreate(user));
        verify(service, times(0)).findAllByRoleAndAccountId(any(), any());
    }

    @ParameterizedTest
    @MethodSource("getUserRolesThatAnAdminInnerGroupCanCreateAndUpdate")
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldUpdateUser_whenRoleAdminWeightCreatedUserLess(Role role, String idGroup) {
        newUser.setRole(role);
        user.getGroup().setId(UUID.fromString(idGroup));
        userFromDatabase.setRole(role);
        userFromDatabase.getGroup().setId(UUID.fromString(idGroup));
        when(service.findAllByRoleAndAccount_IdAndIdNot(any(), any(), any())).thenReturn(List.of(oldUser));
        when(service.findById(newUser.getId())).thenReturn(userFromDatabase);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        assertTrue(validator.beforeUpdate(newUser));
    }

    @ParameterizedTest
    @MethodSource("getUserRolesThatAnAdminNotInnerGroupCanCreateAndUpdate")
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeCreate_shouldCreateUser_whenRoleAdminNotInnerWeightCreatedUserLess(Role role, String idGroup) {
        user.setRole(role);
        user.getGroup().setId(UUID.fromString(idGroup));
        when(roleService.getAccountOwnerRole())
                .thenReturn(roleAccountOwner);
        assertTrue(validator.beforeCreate(user));
        verify(service, times(0)).findAllByRoleAndAccountId(any(), any());
    }

    @ParameterizedTest
    @MethodSource("getUserRolesThatAnAdminNotInnerGroupCanCreateAndUpdate")
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeUpdate_shouldUpdateUser_whenRoleAdminNotInnerWeightCreatedUserThan(Role role, String idGroup) {
        newUser.setRole(role);
        user.getGroup().setId(UUID.fromString(idGroup));
        userFromDatabase.setRole(role);
        userFromDatabase.getGroup().setId(UUID.fromString(idGroup));
        when(service.findAllByRoleAndAccount_IdAndIdNot(any(), any(), any())).thenReturn(List.of(oldUser));
        when(service.findById(newUser.getId())).thenReturn(userFromDatabase);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        assertTrue(validator.beforeUpdate(newUser));
    }

    @ParameterizedTest
    @MethodSource("getUserRolesThatAnAdminNotInnerGroupCanCreateAndUpdate")
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldLogicalValidationException_whenRoleExecutorInnerGroupInnerWeightCreatedUserThan(Role role, String idGroup) {
        user.setRole(role);
        user.getGroup().setId(UUID.fromString(idGroup));
        when(roleService.getAccountOwnerRole())
                .thenReturn(roleAccountOwner);
        assertThrows(LogicalValidationException.class,
                () -> validator.beforeUpdate(user));
    }

    @ParameterizedTest
    @MethodSource("getUserRolesThatAnExecutorInnerGroupNotCanCreateAndUpdate")
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldLogicalValidationException_whenRoleExecutorInnerGroupInnerWeightCreatedUserThan(Role role, String idGroup) {
        newUser.setRole(role);
        user.getGroup().setId(UUID.fromString(idGroup));
        userFromDatabase.setRole(role);
        userFromDatabase.getGroup().setId(UUID.fromString(idGroup));
        when(service.findAllByRoleAndAccount_IdAndIdNot(any(), any(), any())).thenReturn(List.of(oldUser));
        when(service.findById(newUser.getId())).thenReturn(userFromDatabase);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        assertThrows(LogicalValidationException.class,
                () -> validator.beforeUpdate(newUser));
    }

    @ParameterizedTest
    @MethodSource("getUserRolesThatAnExecutorNotInnerGroupNotCanCreateAndUpdate")
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeCreate_shouldLogicalValidationException_whenRoleExecutorNotInnerGroupInnerWeightCreatedUserThan(Role role, String idGroup) {
        user.setRole(role);
        user.getGroup().setId(UUID.fromString(idGroup));
        when(roleService.getAccountOwnerRole())
                .thenReturn(roleAccountOwner);
        assertThrows(LogicalValidationException.class,
                () -> validator.beforeUpdate(user));
    }

    @ParameterizedTest
    @MethodSource("getUserRolesThatAnExecutorNotInnerGroupNotCanCreateAndUpdate")
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeUpdate_shouldLogicalValidationException_whenRoleExecutorNotInnerGroupInnerWeightCreatedUserThan(Role role, String idGroup) {
        newUser.setRole(role);
        user.getGroup().setId(UUID.fromString(idGroup));
        userFromDatabase.setRole(role);
        userFromDatabase.getGroup().setId(UUID.fromString(idGroup));
        when(service.findAllByRoleAndAccount_IdAndIdNot(any(), any(), any())).thenReturn(List.of(oldUser));
        when(service.findById(newUser.getId())).thenReturn(userFromDatabase);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        assertThrows(LogicalValidationException.class,
                () -> validator.beforeUpdate(newUser));
    }

    private static Stream<Arguments> getUserRolesThatAnAccountOwnerCanCreate() {
        return Stream.of(
                Arguments.of(roleAdmin),
                Arguments.of(roleExecutor),
                Arguments.of(roleAuthor),
                Arguments.of(roleObserver)
        );
    }



    private static Stream<Arguments> getUserRolesThatAnAccountOwnerCanUpdate() {
        return Stream.of(
                Arguments.of(roleAccountOwner),
                Arguments.of(roleAdmin),
                Arguments.of(roleExecutor),
                Arguments.of(roleAuthor),
                Arguments.of(roleObserver)
        );
    }

    private static Stream<Arguments> getUserRolesThatAnAdminInnerGroupCanCreateAndUpdate() {
        return Stream.of(
                Arguments.of(roleAdmin, GROUP_1_ID),
                Arguments.of(roleAdmin, GROUP_2_ID),
                Arguments.of(roleExecutor, GROUP_1_ID),
                Arguments.of(roleExecutor, GROUP_2_ID),
                Arguments.of(roleAuthor, GROUP_1_ID),
                Arguments.of(roleAuthor, GROUP_2_ID),
                Arguments.of(roleObserver, GROUP_1_ID),
                Arguments.of(roleObserver, GROUP_2_ID)
        );
    }

    private static Stream<Arguments> getUserRolesThatAnAdminNotInnerGroupCanCreateAndUpdate() {
        return Stream.of(
                Arguments.of(roleAdmin, GROUP_2_ID),
                Arguments.of(roleExecutor, GROUP_2_ID),
                Arguments.of(roleAuthor, GROUP_2_ID),
                Arguments.of(roleObserver, GROUP_2_ID)
        );
    }

    private static Stream<Arguments> getUserRolesThatAnExecutorInnerGroupNotCanCreateAndUpdate() {
        return Stream.of(
                Arguments.of(roleAccountOwner, GROUP_1_ID),
                Arguments.of(roleAccountOwner, GROUP_2_ID),
                Arguments.of(roleAdmin, GROUP_1_ID),
                Arguments.of(roleAdmin, GROUP_2_ID)
        );
    }

    private static Stream<Arguments> getUserRolesThatAnExecutorNotInnerGroupNotCanCreateAndUpdate() {
        return Stream.of(
                Arguments.of(roleAccountOwner, GROUP_1_ID),
                Arguments.of(roleAccountOwner, GROUP_2_ID),
                Arguments.of(roleAdmin, GROUP_1_ID),
                Arguments.of(roleAdmin, GROUP_2_ID),
                Arguments.of(roleExecutor, GROUP_1_ID),
                Arguments.of(roleAuthor, GROUP_1_ID),
                Arguments.of(roleObserver, GROUP_1_ID)
        );
    }


}