package ru.itterminal.botdesk.aau.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.atMost;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static ru.itterminal.botdesk.aau.service.CrudServiceWithAccount.FIND_INVALID_MESSAGE_WITH_ACCOUNT;
import static ru.itterminal.botdesk.security.config.TestSecurityConfig.ACCOUNT_1_ID;
import static ru.itterminal.botdesk.security.config.TestSecurityConfig.EMAIL_1;
import static ru.itterminal.botdesk.security.config.TestSecurityConfig.INNER_GROUP_ID;

import java.io.IOException;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import io.jsonwebtoken.JwtException;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.aau.repository.UserRepository;
import ru.itterminal.botdesk.aau.service.validator.UserOperationValidator;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.exception.FailedSaveEntityException;
import ru.itterminal.botdesk.commons.service.CrudService;
import ru.itterminal.botdesk.integration.across_modules.CompletedVerificationAccount;
import ru.itterminal.botdesk.integration.email.SenderEmailViaSMTPServer;
import ru.itterminal.botdesk.security.jwt.JwtProvider;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.security.jwt.JwtUserBuilder;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {JwtProvider.class, UserServiceImpl.class, BCryptPasswordEncoder.class})
@TestPropertySource(properties = {"jwt.token.secret=ksedtob", "jwt.token.expired=8640000", "jwt.token.prefix=Bearer"})
class UserServiceImplTest {

    private interface MockCompletedVerificationAccount extends CompletedVerificationAccount {
    }

    @MockBean
    private UserRepository userRepository;

    @MockBean
    private RoleServiceImpl roleService;

    @MockBean
    private UserOperationValidator validator;

    @MockBean
    private SenderEmailViaSMTPServer senderEmailViaSMTPServer;

    @MockBean(name = "ticketTypeServiceImpl")
    private CompletedVerificationAccount ticketTypeServiceImpl;

    @MockBean(name = "ticketStatusServiceImpl")
    private MockCompletedVerificationAccount ticketStatusServiceImpl;

    @MockBean
    private JwtUserBuilder jwtUserBuilder;

    @Autowired
    private BCryptPasswordEncoder encoder;

    @Autowired
    private JwtProvider jwtProvider;

    @Autowired
    private UserServiceImpl service;

    private static final UUID USER_ID = UUID.fromString("41ca3b9a-8e94-42a0-acbd-a2d3756af379");
    private User user;
    private User userFromDatabase;
    private JwtUser jwtUser;
    private final Role roleAdmin = new Role(Roles.ADMIN.toString(), Roles.ADMIN.getWeight());
    private final Role roleAccountOwner = new Role(Roles.ACCOUNT_OWNER.toString(), Roles.ACCOUNT_OWNER.getWeight());
    private final UserTestHelper userTestHelper = new UserTestHelper();

    @BeforeEach
    void setUpBeforeEach() {
        Account account = new Account();
        account.setId(UUID.fromString(ACCOUNT_1_ID));
        Group group = Group.builder()
                .id(UUID.fromString(INNER_GROUP_ID))
                .isInner(true)
                .build();
        String PASSWORD = "12345";
        String SOME_TEST_TOKEN = "5423198156154519813598481";
        user = User
                .builder()
                .email(EMAIL_1)
                .password(PASSWORD)
                .account(account)
                .group(group)
                .emailVerificationToken(SOME_TEST_TOKEN)
                .isArchived(false)
                .role(roleAdmin)
                .build();
        user.setId(USER_ID);
        userFromDatabase = User
                .builder()
                .email(EMAIL_1)
                .password(PASSWORD)
                .account(account)
                .group(group)
                .isArchived(false)
                .role(roleAdmin)
                .build();
        userFromDatabase.setId(USER_ID);
        jwtUser = JwtUser.builder()
                .id(user.getId())
                .accountId(user.getAccount().getId())
                .groupId(user.getGroup().getId())
                .isInnerGroup(user.getGroup().getIsInner())
                .weightRole(user.getRole().getWeight())
                .username(user.getEmail())
                .password(user.getPassword())
                .authorities(List.of(new SimpleGrantedAuthority(user.getRole().getName())))
                .enabled(true)
                .build();
        when(jwtUserBuilder.getJwtUser()).thenReturn(jwtUser);
    }

    @Test
    void create_shouldCreateUser_whenPassedValidDataWithRoleAccountOwner() {
        when(validator.logicalValidationBeforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(userRepository.create(any())).thenReturn(user);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        user.setRole(roleAccountOwner);
        User createdUser = service.create(user);
        assertEquals(createdUser, user);
        verify(validator, times(1)).logicalValidationBeforeCreate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(userRepository, times(1)).create(any());
    }

    @Test
    void create_shouldCreateUser_whenPassedValidDataWithRoleAdmin() {
        when(validator.logicalValidationBeforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(userRepository.create(any())).thenReturn(user);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        User createdUser = service.create(user);
        assertEquals(createdUser, user);
        verify(validator, times(1)).logicalValidationBeforeCreate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(userRepository, times(1)).create(any());
    }

    @Test
    void update_shouldUpdateUser_whenPassedValidData() {
        when(validator.logicalValidationBeforeUpdate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(user));
        when(userRepository.update(any())).thenReturn(user);
        User createdUser = service.update(user);
        assertEquals(createdUser, user);
        verify(validator, times(1)).logicalValidationBeforeUpdate(any());
        verify(validator, times(0)).checkUniqueness(any());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findByIdAndAccountId(any(), any());
        verify(userRepository, times(1)).update(any());
    }

    @Test
    void update_shouldGetEntityNotExistException_whenUserIdNotExistInDatabase() {
        when(validator.logicalValidationBeforeUpdate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(userRepository.existsById(any())).thenReturn(false);
        when(userRepository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(user));
        when(userRepository.update(any())).thenReturn(user);
        Throwable throwable = assertThrows(EntityNotExistException.class, () -> service.update(user));
        var expectedMessage = String.format(FIND_INVALID_MESSAGE_WITH_ACCOUNT, "id and accountId", user.getId(),
                                            user.getAccount().getId()
        );
        var actualMessage = throwable.getMessage();
        assertEquals(expectedMessage, actualMessage);
        verify(validator, times(1)).logicalValidationBeforeUpdate(any());
        verify(validator, times(0)).checkUniqueness(any());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(0)).findByIdAndAccountId(any(), any());
        verify(userRepository, times(0)).update(any());
    }

    @Test
    void update_shouldGetOptimisticLockingFailureException_whenPassedInvalidVersionOfUser() {
        String message = String.format(CrudService.VERSION_INVALID_MESSAGE, user.getId());
        when(validator.logicalValidationBeforeUpdate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(user));
        when(userRepository.update(any())).thenThrow(new OptimisticLockingFailureException(message));
        Throwable throwable = assertThrows(OptimisticLockingFailureException.class, () -> service.update(user));
        assertEquals(message, throwable.getMessage());
        verify(validator, times(1)).logicalValidationBeforeUpdate(any());
        verify(validator, times(0)).checkUniqueness(any());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findByIdAndAccountId(any(), any());
        verify(userRepository, times(1)).update(any());
    }

    @Test
    void findByIdAndAccountId_shouldGetUser_whenPassedValidData() {
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(user));
        assertNotNull(service.findByIdAndAccountId(UUID.randomUUID()));
        verify(userRepository, times(1)).findByIdAndAccountId(any(), any());
    }

    @Test
    void verifyEmailToken_shouldUpdateEmailVerificationStatus_whenTokenIsValid() throws IOException {
        String token = jwtProvider.createTokenWithUserId(USER_ID);
        user.setEmailVerificationToken(token);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        user.setEmailVerificationStatus(true);
        when(userRepository.save(any())).thenReturn(user);
        service.verifyEmailTokenOfAccountOwner(token);
        assertTrue(user.getEmailVerificationStatus());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(1)).save(any());
        verify(ticketTypeServiceImpl, atMost(1)).actionAfterCompletedVerificationAccount(any());
        verify(ticketStatusServiceImpl, atMost(1)).actionAfterCompletedVerificationAccount(any());
    }

    @Test
    void verifyEmailToken_shouldGetJwtException_whenEmailVerificationTokenInDatabaseIsNull() {
        String token = jwtProvider.createTokenWithUserId(USER_ID);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        user.setEmailVerificationStatus(true);
        when(userRepository.save(any())).thenReturn(user);
        Throwable throwable = assertThrows(JwtException.class, () -> service.verifyEmailTokenOfAccountOwner(token));
        assertEquals(UserServiceImpl.NOT_FOUND_USER_BY_EMAIL_VERIFICATION_TOKEN, throwable.getMessage());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(0)).save(any());
        verify(ticketTypeServiceImpl, times(0)).actionAfterCompletedVerificationAccount(any());
        verify(ticketStatusServiceImpl, times(0)).actionAfterCompletedVerificationAccount(any());
    }

    @Test
    void verifyEmailToken_shouldGetJwtException_whenEmailVerificationTokenInDatabaseIsNotEqualPassedToken() {
        String token = jwtProvider.createTokenWithUserId(USER_ID);
        String emailVerificationToken = jwtProvider.createTokenWithUserId(UUID.randomUUID());
        user.setEmailVerificationToken(emailVerificationToken);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        Throwable throwable = assertThrows(JwtException.class, () -> service.verifyEmailTokenOfAccountOwner(token));
        assertEquals(UserServiceImpl.NOT_FOUND_USER_BY_EMAIL_VERIFICATION_TOKEN, throwable.getMessage());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(0)).save(any());
        verify(ticketTypeServiceImpl, times(0)).actionAfterCompletedVerificationAccount(any());
        verify(ticketStatusServiceImpl, times(0)).actionAfterCompletedVerificationAccount(any());
    }

    @Test
    void verifyEmailToken_shouldGetFailedSaveEntityException_whenFailedSaveAfterVerifyEmailToken() {
        String token = jwtProvider.createTokenWithUserId(USER_ID);
        user.setEmailVerificationToken(token);
        when(userRepository.existsById(any())).thenReturn(true);
        user.setEmailVerificationStatus(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        when(userRepository.save(any())).thenReturn(User.builder().emailVerificationStatus(false).build());
        Throwable throwable = assertThrows(FailedSaveEntityException.class, () -> service.verifyEmailTokenOfAccountOwner(token));
        assertEquals(UserServiceImpl.FAILED_SAVE_USER_AFTER_VERIFY_EMAIL_TOKEN, throwable.getMessage());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(1)).save(any());
        verify(ticketTypeServiceImpl, times(0)).actionAfterCompletedVerificationAccount(any());
        verify(ticketStatusServiceImpl, times(0)).actionAfterCompletedVerificationAccount(any());
    }

    @Test
    void verifyEmailToken_shouldGetJwtException_whenTokenIsNull() {
        Throwable throwable = assertThrows(JwtException.class, () -> service.verifyEmailTokenOfAccountOwner(null));
        assertEquals("JWT String argument cannot be null or empty.", throwable.getMessage());
        verify(userRepository, times(0)).existsById(any());
        verify(userRepository, times(0)).findById(any());
        verify(userRepository, times(0)).save(any());
        verify(ticketTypeServiceImpl, times(0)).actionAfterCompletedVerificationAccount(any());
        verify(ticketStatusServiceImpl, times(0)).actionAfterCompletedVerificationAccount(any());
    }

    @Test
    void requestPasswordReset_shouldUpdatePasswordResetToken_whenEmailIsValid() {
        when(userRepository.getByEmail(any())).thenReturn(Optional.of(user));
        service.requestPasswordReset(EMAIL_1);
        verify(userRepository, times(1)).getByEmail(any());
        verify(userRepository, times(1)).save(any());
    }

    @Test
    void requestPasswordReset_shouldGetEntityNotExistException_whenEmailIsInvalid() {
        when(userRepository.getByEmail(any())).thenReturn(Optional.empty());
        assertThrows(EntityNotExistException.class, () -> service.requestPasswordReset(EMAIL_1));
        verify(userRepository, times(1)).getByEmail(any());
        verify(userRepository, times(0)).save(any());
    }

    @Test
    void resetPassword_shouldResetPassword_whenPassedDataIsValid() {
        String newPassword = "newPassword";
        String token = jwtProvider.createTokenWithUserId(USER_ID);
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
        String token = jwtProvider.createTokenWithUserId(USER_ID);
        String tokenFromDatabase = jwtProvider.createTokenWithUserId(UUID.randomUUID());
        user.setPasswordResetToken(tokenFromDatabase);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        Throwable throwable = assertThrows(JwtException.class, () -> service.resetPassword(token, newPassword));
        assertEquals(UserServiceImpl.NOT_FOUND_USER_BY_RESET_PASSWORD_TOKEN, throwable.getMessage());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(0)).save(any());
    }

    @Test
    void resetPassword_shouldGetJwtException_whenPasswordTokenInDatabaseIsNull() {
        String newPassword = "newPassword";
        String token = jwtProvider.createTokenWithUserId(USER_ID);
        user.setPasswordResetToken(null);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        Throwable throwable = assertThrows(JwtException.class, () -> service.resetPassword(token, newPassword));
        assertEquals(UserServiceImpl.NOT_FOUND_USER_BY_RESET_PASSWORD_TOKEN, throwable.getMessage());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(0)).save(any());
    }

    @Test
    void resetPassword_shouldGetFailedSaveEntityException_whenFailedSaveAfterResetPassword() {
        String newPassword = "newPassword";
        String token = jwtProvider.createTokenWithUserId(USER_ID);
        user.setPasswordResetToken(token);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        when(userRepository.save(any())).thenReturn(userFromDatabase);
        Throwable throwable =
                assertThrows(FailedSaveEntityException.class, () -> service.resetPassword(token, newPassword));
        assertEquals(UserServiceImpl.FAILED_SAVE_USER_AFTER_RESET_PASSWORD, throwable.getMessage());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(1)).save(any());
    }

    @Test
    void requestUpdateEmailOfAccountOwner_shouldUpdateEmailVerificationToken_whenNewEmailIsValid() {
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(user));
        when(userRepository.getByEmail(any())).thenReturn(Optional.of(user));
        service.requestUpdateEmailOfAccountOwner(EMAIL_1);
        verify(jwtUserBuilder, times(2)).getJwtUser();
        verify(userRepository, times(1)).save(any());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findByIdAndAccountId(any(), any());
        verify(senderEmailViaSMTPServer, times(1)).createEmail(any(), any(), any());
        verify(senderEmailViaSMTPServer, times(1)).sendEmail(any());
    }

    @Test
    void updateEmailOfAccountOwner_shouldUpdateEmailOfAccountOwner_whenPassedValidToken() {
        var user = userTestHelper.getRandomValidEntity();
        var token = jwtProvider.createTokenWithJwtUser(EMAIL_1, jwtUser);
        user.setEmailVerificationToken(token);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(user));
        when(userRepository.save(any())).thenReturn(userFromDatabase);
        service.updateEmailOfAccountOwner(token);
        assertNull(user.getEmailVerificationToken());
        assertEquals(EMAIL_1, user.getEmail());
        verify(jwtUserBuilder, times(2)).getJwtUser();
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findByIdAndAccountId(any(), any());
        verify(userRepository, times(1)).save(any());
    }

}
