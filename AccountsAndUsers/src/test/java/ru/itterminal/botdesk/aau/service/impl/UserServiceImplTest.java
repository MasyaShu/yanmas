package ru.itterminal.botdesk.aau.service.impl;

import static java.lang.String.format;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static ru.itterminal.botdesk.aau.model.Roles.ACCOUNT_OWNER;
import static ru.itterminal.botdesk.aau.model.Roles.ADMIN;
import static ru.itterminal.botdesk.aau.service.impl.UserServiceImpl.FAILED_SAVE_USER_AFTER_RESET_PASSWORD;
import static ru.itterminal.botdesk.aau.service.impl.UserServiceImpl.FAILED_SAVE_USER_AFTER_VERIFY_EMAIL_TOKEN;
import static ru.itterminal.botdesk.aau.service.impl.UserServiceImpl.NOT_FOUND_USER_BY_EMAIL_VERIFICATION_TOKEN;
import static ru.itterminal.botdesk.aau.service.impl.UserServiceImpl.NOT_FOUND_USER_BY_RESET_PASSWORD_TOKEN;
import static ru.itterminal.botdesk.commons.service.CrudService.FIND_INVALID_MESSAGE;
import static ru.itterminal.botdesk.commons.service.CrudService.VERSION_INVALID_MESSAGE;
import static ru.itterminal.botdesk.config.TestSecurityConfig.ACCOUNT_1_ID;
import static ru.itterminal.botdesk.config.TestSecurityConfig.EMAIL_1;
import static ru.itterminal.botdesk.config.TestSecurityConfig.GROUP_1_ID;

import java.util.Optional;
import java.util.UUID;

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
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.repository.UserRepository;
import ru.itterminal.botdesk.aau.service.validator.UserOperationValidator;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.exception.FailedSaveEntityException;
import ru.itterminal.botdesk.jwt.JwtProvider;

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

    @Autowired
    private BCryptPasswordEncoder encoder = new BCryptPasswordEncoder();

    @Autowired
    private JwtProvider jwtProvider = new JwtProvider();

    @Autowired
    private UserServiceImpl service;

    private static final UUID USER_ID = UUID.fromString("41ca3b9a-8e94-42a0-acbd-a2d3756af379");
    private static String PASSWORD = "12345";
    private User user;
    private User userFromInDatabase;
    private Account account;
    private Group group;
    private Role roleAdmin = new Role(ADMIN.toString(), ADMIN.getWeight());
    private Role roleAccountOwner = new Role(ACCOUNT_OWNER.toString(), ACCOUNT_OWNER.getWeight());

    @BeforeEach
    void setUpBeforeEach() {
        account = new Account();
        account.setId(UUID.fromString(ACCOUNT_1_ID));
        group = new Group();
        group.setId(UUID.fromString(GROUP_1_ID));
        user = new User().builder()
                .email(EMAIL_1)
                .password(PASSWORD)
                .account(account)
                .ownGroup(group)
                .isArchived(false)
                .role(roleAdmin)
                .build();
        user.setId(USER_ID);
        userFromInDatabase = new User().builder()
                .email(EMAIL_1)
                .password(PASSWORD)
                .account(account)
                .ownGroup(group)
                .isArchived(false)
                .role(roleAdmin)
                .build();
        userFromInDatabase.setId(USER_ID);
    }

    @Test
    public void create_shouldCreateUser_whenPassedValidDataWithRoleAccountOwner() {
        when(validator.beforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(userRepository.create(any())).thenReturn(user);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        user.setRole(roleAccountOwner);
        User createdUser = service.create(user);
        assertTrue(user.equals(createdUser));
        verify(validator, times(1)).beforeCreate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(userRepository, times(1)).create(any());

    }

    @Test
    public void create_shouldCreateUser_whenPassedValidDataWithRoleAdmin() {
        when(validator.beforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(userRepository.create(any())).thenReturn(user);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        User createdUser = service.create(user);
        assertTrue(user.equals(createdUser));
        verify(validator, times(1)).beforeCreate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(userRepository, times(1)).create(any());

    }

    @Test
    public void update_shouldUpdateUser_whenPassedValidData() {
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        when(userRepository.update(any())).thenReturn(user);
        User createdUser = service.update(user);
        assertTrue(user.equals(createdUser));
        verify(validator, times(1)).beforeUpdate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(1)).update(any());
    }

    @Test
    public void update_shouldGetEntityNotExistException_whenUserIdNotExistInDatabase() {
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(userRepository.existsById(any())).thenReturn(false);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        when(userRepository.update(any())).thenReturn(user);
        Throwable throwable = assertThrows(EntityNotExistException.class, ()-> service.update(user));
        assertEquals(format(FIND_INVALID_MESSAGE, "id", user.getId()), throwable.getMessage());
        verify(validator, times(1)).beforeUpdate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(0)).findById(any());
        verify(userRepository, times(0)).update(any());
    }

    @Test
    public void update_shouldGetOptimisticLockingFailureException_whenPassedInvalidVersionOfUser() {
        String message = format(VERSION_INVALID_MESSAGE, user.getId());
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        when(userRepository.update(any())).thenThrow(new OptimisticLockingFailureException(message));
        Throwable throwable = assertThrows(OptimisticLockingFailureException.class, ()-> service.update(user));
        assertEquals(message, throwable.getMessage());
        verify(validator, times(1)).beforeUpdate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(1)).update(any());
    }

    @Test
    public void findByEmail_shouldGetEntityNotExistException_whenEmailIsNull() {
        assertThrows(EntityNotExistException.class, () -> service.findByEmail(null));
        verify(userRepository, times(0)).getByEmail(any());
    }

    @Test
    public void findByEmail_shouldGetEntityNotExistException_whenEmailIsEmpty() {
        assertThrows(EntityNotExistException.class, () -> service.findByEmail(""));
        verify(userRepository, times(0)).getByEmail(any());
    }

    @Test
    public void findByUniqueFields_shouldGetEntityNotExistException_whenUserIsNull() {
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(null));
        verify(userRepository, times(0)).getByEmailAndIdNot(any(), any());
    }

    @Test
    public void findByUniqueFields_shouldGetEntityNotExistException_whenUserEmailIsNull() {
        user.setEmail(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(user));
        verify(userRepository, times(0)).getByEmailAndIdNot(any(), any());
    }

    @Test
    public void findByUniqueFields_shouldGetEntityNotExistException_whenUserIdIsNull() {
        user.setId(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(user));
        verify(userRepository, times(0)).getByEmailAndIdNot(any(), any());
    }

    @Test
    public void findByIdAndAccountId_shouldGetEntityNotExistException_whenUserIdIsNull() {
        assertThrows(EntityNotExistException.class, () -> service.findByIdAndAccountId(null, account.getId()));
        verify(userRepository, times(0)).getByIdAndAccount_Id(any(), any());
    }

    @Test
    public void findByIdAndAccountId_shouldGetEntityNotExistException_whenAccountIdIsNull() {
        assertThrows(EntityNotExistException.class, () -> service.findByIdAndAccountId(user.getId(), null));
        verify(userRepository, times(0)).getByIdAndAccount_Id(any(), any());
    }

    @Test
    public void findAllByRoleAndIdNot_shouldGetEntityNotExistException_whenRoleIsNull() {
        assertThrows(EntityNotExistException.class, () -> service.findAllByRoleAndIdNot(null, user.getId()));
        verify(userRepository, times(0)).findAllByRoleAndIdNot(any(), any());
    }

    @Test
    public void findAllByRoleAndIdNot_shouldGetEntityNotExistException_whenUserIdIsNull() {
        assertThrows(EntityNotExistException.class, () -> service.findAllByRoleAndIdNot(roleAdmin, null));
        verify(userRepository, times(0)).findAllByRoleAndIdNot(any(), any());
    }

    @Test
    public void findAllByRole_shouldGetEntityNotExistException_whenRoleIsNull() {
        assertThrows(EntityNotExistException.class, () -> service.findAllByRole(null));
        verify(userRepository, times(0)).findAllByRole(any());
    }

    @Test
    public void verifyEmailToken_shouldUpdateEmailVerificationStatus_whenTokenIsValid() {
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
    public void verifyEmailToken_shouldGetJwtException_whenEmailVerificationTokenInDatabaseIsNull() {
        String token = jwtProvider.createToken(USER_ID);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        user.setEmailVerificationStatus(true);
        when(userRepository.save(any())).thenReturn(user);
        Throwable throwable = assertThrows(JwtException.class, () -> service.verifyEmailToken(token));
        assertTrue(throwable.getMessage().equals(NOT_FOUND_USER_BY_EMAIL_VERIFICATION_TOKEN));
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(0)).save(any());

    }

    @Test
    public void verifyEmailToken_shouldGetJwtException_whenEmailVerificationTokenInDatabaseIsNotEqualPassedToken() {
        String token = jwtProvider.createToken(USER_ID);
        String emailVerificationToken = jwtProvider.createToken(UUID.randomUUID());
        user.setEmailVerificationToken(emailVerificationToken);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        Throwable throwable = assertThrows(JwtException.class, () -> service.verifyEmailToken(token));
        assertTrue(throwable.getMessage().equals(NOT_FOUND_USER_BY_EMAIL_VERIFICATION_TOKEN));
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(0)).save(any());
    }

    @Test
    public void verifyEmailToken_shouldGetFailedSaveEntityException_whenFailedSaveAfterVerifyEmailToken() {
        String token = jwtProvider.createToken(USER_ID);
        user.setEmailVerificationToken(token);
        when(userRepository.existsById(any())).thenReturn(true);
        user.setEmailVerificationStatus(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        when(userRepository.save(any())).thenReturn(null);
        Throwable throwable = assertThrows(FailedSaveEntityException.class, () -> service.verifyEmailToken(token));
        assertTrue(throwable.getMessage().equals(FAILED_SAVE_USER_AFTER_VERIFY_EMAIL_TOKEN));
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(1)).save(any());
    }

    @Test
    public void verifyEmailToken_shouldGetJwtException_whenTokenIsNull() {
        String token = null;
        Throwable throwable = assertThrows(JwtException.class, () -> service.verifyEmailToken(token));
        assertEquals("JWT String argument cannot be null or empty.", throwable.getMessage());
        verify(userRepository, times(0)).existsById(any());
        verify(userRepository, times(0)).findById(any());
        verify(userRepository, times(0)).save(any());
    }

    @Test
    public void requestPasswordReset_shouldUpdatePasswordResetToken_whenEmailIsValid() {
        when(userRepository.getByEmail(any())).thenReturn(Optional.of(user));
        service.requestPasswordReset(EMAIL_1);
        verify(userRepository, times(1)).getByEmail(any());
        verify(userRepository, times(1)).save(any());
    }

    @Test
    public void requestPasswordReset_shouldGetEntityNotExistException_whenEmailIsInvalid() {
        when(userRepository.getByEmail(any())).thenReturn(Optional.empty());
        assertThrows(EntityNotExistException.class, ()-> service.requestPasswordReset(EMAIL_1));
        verify(userRepository, times(1)).getByEmail(any());
        verify(userRepository, times(0)).save(any());
    }

    @Test
    public void resetPassword_shouldResetPassword_whenPassedDataIsValid() {
        String newPassword = "newPassword";
        String token = jwtProvider.createToken(USER_ID);
        user.setPasswordResetToken(token);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        user.setEmailVerificationStatus(true);
        userFromInDatabase.setPassword(encoder.encode(newPassword));
        when(userRepository.save(any())).thenReturn(userFromInDatabase);
        service.resetPassword(token, newPassword);
        assertTrue(user.getEmailVerificationStatus());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(1)).save(any());

    }

    @Test
    public void resetPassword_shouldGetJwtException_whenNotFoundUserByResetPasswordToken() {
        String newPassword = "newPassword";
        String token = jwtProvider.createToken(USER_ID);
        String tokenFromDatabase = jwtProvider.createToken(UUID.randomUUID());
        user.setPasswordResetToken(tokenFromDatabase);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        Throwable throwable = assertThrows(JwtException.class, () -> service.resetPassword(token, newPassword));
        assertTrue(throwable.getMessage().equals(NOT_FOUND_USER_BY_RESET_PASSWORD_TOKEN));
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(0)).save(any());
    }

    @Test
    public void resetPassword_shouldGetJwtException_whenPasswordTokenInDatabaseIsNull() {
        String newPassword = "newPassword";
        String token = jwtProvider.createToken(USER_ID);
        String tokenFromDatabase = null;
        user.setPasswordResetToken(tokenFromDatabase);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        Throwable throwable = assertThrows(JwtException.class, () -> service.resetPassword(token, newPassword));
        assertTrue(throwable.getMessage().equals(NOT_FOUND_USER_BY_RESET_PASSWORD_TOKEN));
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(0)).save(any());
    }

    @Test
    public void resetPassword_shouldGetFailedSaveEntityException_whenFailedSaveAfterResetPassword() {
        String newPassword = "newPassword";
        String token = jwtProvider.createToken(USER_ID);
        String tokenFromDatabase = token;
        user.setPasswordResetToken(tokenFromDatabase);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        when(userRepository.save(any())).thenReturn(userFromInDatabase);
        Throwable throwable = assertThrows(FailedSaveEntityException.class, () -> service.resetPassword(token, newPassword));
        assertTrue(throwable.getMessage().equals(FAILED_SAVE_USER_AFTER_RESET_PASSWORD));
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(1)).save(any());
    }

}