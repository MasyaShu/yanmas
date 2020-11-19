package ru.itterminal.botdesk.aau.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import io.jsonwebtoken.JwtException;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.repository.UserRepository;
import ru.itterminal.botdesk.aau.service.validator.UserOperationValidator;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.exception.FailedSaveEntityException;
import ru.itterminal.botdesk.commons.service.CrudService;
import ru.itterminal.botdesk.integration.aws.SenderEmailViaAwsSes;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.security.jwt.JwtProvider;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {JwtProvider.class, UserServiceImpl.class, BCryptPasswordEncoder.class})
@TestPropertySource(properties = {"jwt.token.secret=ksedtob", "jwt.token.expired=8640000", "jwt.token.prefix=Bearer"})
class UserServiceImplTest {

    @MockBean
    private UserRepository userRepository;

    @MockBean
    private RoleServiceImpl roleService;

    @MockBean
    private UserOperationValidator validator;

    @SuppressWarnings("unused")
    @MockBean
    private SenderEmailViaAwsSes.MailSenderViaAwsSesMessagingGateway gateway;

    @Autowired
    private BCryptPasswordEncoder encoder;

    @Autowired
    private JwtProvider jwtProvider;

    @Autowired
    private UserServiceImpl service;

    private static final UUID USER_ID = UUID.fromString("41ca3b9a-8e94-42a0-acbd-a2d3756af379");
    private User user;
    private User userFromDatabase;
    private Account account;
    private final Role roleAdmin = new Role(Roles.ADMIN.toString(), Roles.ADMIN.getWeight());
    private final Role roleAccountOwner = new Role(Roles.ACCOUNT_OWNER.toString(), Roles.ACCOUNT_OWNER.getWeight());

    @BeforeEach
    void setUpBeforeEach() {
        account = new Account();
        account.setId(UUID.fromString(TestSecurityConfig.ACCOUNT_1_ID));
        Group group = new Group();
        group.setId(UUID.fromString(TestSecurityConfig.GROUP_1_ID));
        String PASSWORD = "12345";
        String SOME_TEST_TOKEN = "asdfjhasjdf734y57823asdfsabdfhsabdf";
        user = User
                .builder()
                .email(TestSecurityConfig.EMAIL_1)
                .password(PASSWORD)
                .account(account)
                .ownGroup(group)
                .emailVerificationToken(SOME_TEST_TOKEN)
                .isArchived(false)
                .role(roleAdmin)
                .build();
        user.setId(USER_ID);
        userFromDatabase = User
                .builder()
                .email(TestSecurityConfig.EMAIL_1)
                .password(PASSWORD)
                .account(account)
                .ownGroup(group)
                .isArchived(false)
                .role(roleAdmin)
                .build();
        userFromDatabase.setId(USER_ID);
    }

    @Test
    void create_shouldCreateUser_whenPassedValidDataWithRoleAccountOwner() {
        when(validator.beforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(userRepository.create(any())).thenReturn(user);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        user.setRole(roleAccountOwner);
        User createdUser = service.create(user);
        assertEquals(createdUser, user);
        verify(validator, times(1)).beforeCreate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(userRepository, times(1)).create(any());

    }

    @Test
    void create_shouldCreateUser_whenPassedValidDataWithRoleAdmin() {
        when(validator.beforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(userRepository.create(any())).thenReturn(user);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        User createdUser = service.create(user);
        assertEquals(createdUser, user);
        verify(validator, times(1)).beforeCreate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(userRepository, times(1)).create(any());

    }

    @Test
    void update_shouldUpdateUser_whenPassedValidData() {
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        when(userRepository.update(any())).thenReturn(user);
        User createdUser = service.update(user);
        assertEquals(createdUser, user);
        verify(validator, times(1)).beforeUpdate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(1)).update(any());
    }

    @Test
    void update_shouldGetEntityNotExistException_whenUserIdNotExistInDatabase() {
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(userRepository.existsById(any())).thenReturn(false);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        when(userRepository.update(any())).thenReturn(user);
        Throwable throwable = assertThrows(EntityNotExistException.class, () -> service.update(user));
        assertEquals(String.format(CrudService.FIND_INVALID_MESSAGE, "id", user.getId()), throwable.getMessage());
        verify(validator, times(1)).beforeUpdate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(0)).findById(any());
        verify(userRepository, times(0)).update(any());
    }

    @Test
    void update_shouldGetOptimisticLockingFailureException_whenPassedInvalidVersionOfUser() {
        String message = String.format(CrudService.VERSION_INVALID_MESSAGE, user.getId());
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        when(userRepository.update(any())).thenThrow(new OptimisticLockingFailureException(message));
        Throwable throwable = assertThrows(OptimisticLockingFailureException.class, () -> service.update(user));
        assertEquals(message, throwable.getMessage());
        verify(validator, times(1)).beforeUpdate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(1)).update(any());
    }

    @Test
    void findByEmail_shouldGetEntityNotExistException_whenEmailIsNull() {
        assertThrows(EntityNotExistException.class, () -> service.findByEmail(null));
        verify(userRepository, times(0)).getByEmail(any());
    }

    @Test
    void findByEmail_shouldGetEntityNotExistException_whenEmailIsEmpty() {
        assertThrows(EntityNotExistException.class, () -> service.findByEmail(""));
        verify(userRepository, times(0)).getByEmail(any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserIsNull() {
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(null));
        verify(userRepository, times(0)).getByEmailAndIdNot(any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserEmailIsNull() {
        user.setEmail(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(user));
        verify(userRepository, times(0)).getByEmailAndIdNot(any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserIdIsNull() {
        user.setId(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(user));
        verify(userRepository, times(0)).getByEmailAndIdNot(any(), any());
    }

    @Test
    void findByIdAndAccountId_shouldGetEntityNotExistException_whenUserIdIsNull() {
        UUID accountId = account.getId();
        assertThrows(EntityNotExistException.class, () -> service.findByIdAndAccountId(null, accountId));
        verify(userRepository, times(0)).getByIdAndAccount_Id(any(), any());
    }

    @Test
    void findByIdAndAccountId_shouldGetEntityNotExistException_whenAccountIdIsNull() {
        UUID userId = user.getId();
        assertThrows(EntityNotExistException.class, () -> service.findByIdAndAccountId(userId, null));
        verify(userRepository, times(0)).getByIdAndAccount_Id(any(), any());
    }

    @Test
    void findAllByRoleAndIdNot_shouldGetEntityNotExistException_whenRoleIsNull() {
        UUID userId = user.getId();
        assertThrows(EntityNotExistException.class, () -> service.findAllByRoleAndIdNot(null, userId));
        verify(userRepository, times(0)).findAllByRoleAndIdNot(any(), any());
    }

    @Test
    void findAllByRoleAndIdNot_shouldGetEntityNotExistException_whenUserIdIsNull() {
        assertThrows(EntityNotExistException.class, () -> service.findAllByRoleAndIdNot(roleAdmin, null));
        verify(userRepository, times(0)).findAllByRoleAndIdNot(any(), any());
    }

    @Test
    void findAllByRole_shouldGetEntityNotExistException_whenRoleIsNull() {
        assertThrows(EntityNotExistException.class, () -> service.findAllByRole(null));
        verify(userRepository, times(0)).findAllByRole(any());
    }

    @Test
    void verifyEmailToken_shouldUpdateEmailVerificationStatus_whenTokenIsValid() {
        String token = jwtProvider.createToken(USER_ID);
        user.setEmailVerificationToken(token);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        user.setEmailVerificationStatus(true);
        when(userRepository.save(any())).thenReturn(user);
        service.verifyEmailToken(token);
        assertTrue(user.getEmailVerificationStatus());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(1)).save(any());
    }

    @Test
    void verifyEmailToken_shouldGetJwtException_whenEmailVerificationTokenInDatabaseIsNull() {
        String token = jwtProvider.createToken(USER_ID);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        user.setEmailVerificationStatus(true);
        when(userRepository.save(any())).thenReturn(user);
        Throwable throwable = assertThrows(JwtException.class, () -> service.verifyEmailToken(token));
        Assertions.assertEquals(UserServiceImpl.NOT_FOUND_USER_BY_EMAIL_VERIFICATION_TOKEN, throwable.getMessage());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(0)).save(any());

    }

    @Test
    void verifyEmailToken_shouldGetJwtException_whenEmailVerificationTokenInDatabaseIsNotEqualPassedToken() {
        String token = jwtProvider.createToken(USER_ID);
        String emailVerificationToken = jwtProvider.createToken(UUID.randomUUID());
        user.setEmailVerificationToken(emailVerificationToken);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        Throwable throwable = assertThrows(JwtException.class, () -> service.verifyEmailToken(token));
        Assertions.assertEquals(UserServiceImpl.NOT_FOUND_USER_BY_EMAIL_VERIFICATION_TOKEN, throwable.getMessage());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(0)).save(any());
    }

    @Test
    void verifyEmailToken_shouldGetFailedSaveEntityException_whenFailedSaveAfterVerifyEmailToken() {
        String token = jwtProvider.createToken(USER_ID);
        user.setEmailVerificationToken(token);
        when(userRepository.existsById(any())).thenReturn(true);
        user.setEmailVerificationStatus(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        when(userRepository.save(any())).thenReturn(User.builder().emailVerificationStatus(false).build());
        Throwable throwable = assertThrows(FailedSaveEntityException.class, () -> service.verifyEmailToken(token));
        Assertions.assertEquals(UserServiceImpl.FAILED_SAVE_USER_AFTER_VERIFY_EMAIL_TOKEN, throwable.getMessage());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(1)).save(any());
    }

    @Test
    void verifyEmailToken_shouldGetJwtException_whenTokenIsNull() {
        Throwable throwable = assertThrows(JwtException.class, () -> service.verifyEmailToken(null));
        assertEquals("JWT String argument cannot be null or empty.", throwable.getMessage());
        verify(userRepository, times(0)).existsById(any());
        verify(userRepository, times(0)).findById(any());
        verify(userRepository, times(0)).save(any());
    }

    @Test
    void requestPasswordReset_shouldUpdatePasswordResetToken_whenEmailIsValid() {
        when(userRepository.getByEmail(any())).thenReturn(Optional.of(user));
        service.requestPasswordReset(TestSecurityConfig.EMAIL_1);
        verify(userRepository, times(1)).getByEmail(any());
        verify(userRepository, times(1)).save(any());
    }

    @Test
    void requestPasswordReset_shouldGetEntityNotExistException_whenEmailIsInvalid() {
        when(userRepository.getByEmail(any())).thenReturn(Optional.empty());
        assertThrows(EntityNotExistException.class, () -> service.requestPasswordReset(TestSecurityConfig.EMAIL_1));
        verify(userRepository, times(1)).getByEmail(any());
        verify(userRepository, times(0)).save(any());
    }

    @Test
    void resetPassword_shouldResetPassword_whenPassedDataIsValid() {
        String newPassword = "newPassword";
        String token = jwtProvider.createToken(USER_ID);
        user.setPasswordResetToken(token);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        user.setEmailVerificationStatus(true);
        userFromDatabase.setPassword(encoder.encode(newPassword));
        when(userRepository.save(any())).thenReturn(userFromDatabase);
        service.resetPassword(token, newPassword);
        assertTrue(user.getEmailVerificationStatus());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(1)).save(any());

    }

    @Test
    void resetPassword_shouldGetJwtException_whenNotFoundUserByResetPasswordToken() {
        String newPassword = "newPassword";
        String token = jwtProvider.createToken(USER_ID);
        String tokenFromDatabase = jwtProvider.createToken(UUID.randomUUID());
        user.setPasswordResetToken(tokenFromDatabase);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        Throwable throwable = assertThrows(JwtException.class, () -> service.resetPassword(token, newPassword));
        Assertions.assertEquals(UserServiceImpl.NOT_FOUND_USER_BY_RESET_PASSWORD_TOKEN, throwable.getMessage());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(0)).save(any());
    }

    @Test
    void resetPassword_shouldGetJwtException_whenPasswordTokenInDatabaseIsNull() {
        String newPassword = "newPassword";
        String token = jwtProvider.createToken(USER_ID);
        user.setPasswordResetToken(null);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        Throwable throwable = assertThrows(JwtException.class, () -> service.resetPassword(token, newPassword));
        Assertions.assertEquals(UserServiceImpl.NOT_FOUND_USER_BY_RESET_PASSWORD_TOKEN, throwable.getMessage());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(0)).save(any());
    }

    @Test
    void resetPassword_shouldGetFailedSaveEntityException_whenFailedSaveAfterResetPassword() {
        String newPassword = "newPassword";
        String token = jwtProvider.createToken(USER_ID);
        user.setPasswordResetToken(token);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        when(userRepository.save(any())).thenReturn(userFromDatabase);
        Throwable throwable =
                assertThrows(FailedSaveEntityException.class, () -> service.resetPassword(token, newPassword));
        Assertions.assertEquals(UserServiceImpl.FAILED_SAVE_USER_AFTER_RESET_PASSWORD, throwable.getMessage());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(1)).save(any());
    }
}