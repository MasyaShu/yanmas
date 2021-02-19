package ru.itterminal.botdesk.aau.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static ru.itterminal.botdesk.aau.service.CrudServiceWithAccount.FIND_INVALID_MESSAGE_WITH_ACCOUNT;
import static ru.itterminal.botdesk.security.config.TestSecurityConfig.ACCOUNT_1_ID;
import static ru.itterminal.botdesk.security.config.TestSecurityConfig.EMAIL_1;
import static ru.itterminal.botdesk.security.config.TestSecurityConfig.INNER_GROUP_ID;

import java.io.IOException;
import java.util.Optional;
import java.util.UUID;

import javax.mail.MessagingException;

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
import ru.itterminal.botdesk.commons.exception.AwsSesException;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.exception.FailedSaveEntityException;
import ru.itterminal.botdesk.commons.service.CrudService;
import ru.itterminal.botdesk.integration.across_modules.CompletedVerificationAccount;
import ru.itterminal.botdesk.integration.aws.ses.SenderEmailViaAwsSes;
import ru.itterminal.botdesk.security.jwt.JwtProvider;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.security.jwt.JwtUserBuilder;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {JwtProvider.class, UserServiceImpl.class, BCryptPasswordEncoder.class})
@TestPropertySource(properties = {"jwt.token.secret=ksedtob", "jwt.token.expired=8640000", "jwt.token.prefix=Bearer"})
class UserServiceImplTest {

    public static final String AWS_SES_EXCEPTION = "AWS SES Exception";

    private interface MockCompletedVerificationAccount extends CompletedVerificationAccount {
    }

    @MockBean
    private UserRepository userRepository;

    @MockBean
    private RoleServiceImpl roleService;

    @MockBean
    private UserOperationValidator validator;

    @SuppressWarnings("unused")
    @MockBean
    private SenderEmailViaAwsSes senderEmailViaAwsSes;

    @MockBean(name = "ticketTypeServiceImpl")
    private CompletedVerificationAccount ticketTypeServiceImpl;

    @MockBean(name = "ticketStatusServiceImpl")
    private MockCompletedVerificationAccount ticketStatusServiceImpl;

    @MockBean
    private JwtUserBuilder jwtUserBuilder;

    @MockBean
    private JwtUser jwtUser;

    @Autowired
    private BCryptPasswordEncoder encoder;

    @Autowired
    private JwtProvider jwtProvider;

    @Autowired
    private UserServiceImpl service;

    private static final UUID USER_ID = UUID.fromString("41ca3b9a-8e94-42a0-acbd-a2d3756af379");
    private User user;
    private User userFromDatabase;
    private final Role roleAdmin = new Role(Roles.ADMIN.toString(), Roles.ADMIN.getWeight());
    private final Role roleAccountOwner = new Role(Roles.ACCOUNT_OWNER.toString(), Roles.ACCOUNT_OWNER.getWeight());

    @BeforeEach
    void setUpBeforeEach() {
        Account account = new Account();
        account.setId(UUID.fromString(ACCOUNT_1_ID));
        Group group = new Group();
        group.setId(UUID.fromString(INNER_GROUP_ID));
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
        when(jwtUserBuilder.getJwtUser()).thenReturn(jwtUser);
        when(jwtUser.getAccountId()).thenReturn(user.getAccount().getId());
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
    void create_shouldGetAwsSesException_whenWasErrorWhenCreateEmail() throws IOException, MessagingException {
        when(validator.beforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(userRepository.create(any())).thenReturn(user);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        when(senderEmailViaAwsSes.createEmail(any(), any(), any(), any(), any()))
                .thenThrow(new MessagingException(AWS_SES_EXCEPTION));
        Throwable throwable = assertThrows(AwsSesException.class, () -> service.create(user));
        assertEquals(AWS_SES_EXCEPTION, throwable.getMessage());
        verify(validator, times(1)).beforeCreate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(userRepository, times(1)).create(any());
        verify(senderEmailViaAwsSes, times(1)).createEmail(any(), any(), any(), any(), any());
    }

    @Test
    void create_shouldGetAwsSesException_whenWasErrorWhenSendEmail() {
        when(validator.beforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(userRepository.create(any())).thenReturn(user);
        when(roleService.getAccountOwnerRole()).thenReturn(roleAccountOwner);
        when(senderEmailViaAwsSes.sendEmail(any())).thenThrow(new RuntimeException(AWS_SES_EXCEPTION));
        Throwable throwable = assertThrows(RuntimeException.class, () -> service.create(user));
        assertEquals(AWS_SES_EXCEPTION, throwable.getMessage());
        verify(validator, times(1)).beforeCreate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(userRepository, times(1)).create(any());
        verify(senderEmailViaAwsSes, times(1)).sendEmail(any());
    }

    @Test
    void update_shouldUpdateUser_whenPassedValidData() {
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(user));
        when(userRepository.update(any())).thenReturn(user);
        User createdUser = service.update(user);
        assertEquals(createdUser, user);
        verify(validator, times(1)).beforeUpdate(any());
        verify(validator, times(0)).checkUniqueness(any());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findByIdAndAccountId(any(), any());
        verify(userRepository, times(1)).update(any());
    }

    @Test
    void update_shouldGetEntityNotExistException_whenUserIdNotExistInDatabase() {
        when(validator.beforeUpdate(any())).thenReturn(true);
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
        verify(validator, times(1)).beforeUpdate(any());
        verify(validator, times(0)).checkUniqueness(any());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(0)).findByIdAndAccountId(any(), any());
        verify(userRepository, times(0)).update(any());
    }

    @Test
    void update_shouldGetOptimisticLockingFailureException_whenPassedInvalidVersionOfUser() {
        String message = String.format(CrudService.VERSION_INVALID_MESSAGE, user.getId());
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(user));
        when(userRepository.update(any())).thenThrow(new OptimisticLockingFailureException(message));
        Throwable throwable = assertThrows(OptimisticLockingFailureException.class, () -> service.update(user));
        assertEquals(message, throwable.getMessage());
        verify(validator, times(1)).beforeUpdate(any());
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
        verify(ticketTypeServiceImpl, times(1)).actionAfterCompletedVerificationAccount(any());
        verify(ticketStatusServiceImpl, times(1)).actionAfterCompletedVerificationAccount(any());
    }

    @Test
    void verifyEmailToken_shouldGetJwtException_whenEmailVerificationTokenInDatabaseIsNull() {
        String token = jwtProvider.createToken(USER_ID);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        user.setEmailVerificationStatus(true);
        when(userRepository.save(any())).thenReturn(user);
        Throwable throwable = assertThrows(JwtException.class, () -> service.verifyEmailToken(token));
        assertEquals(UserServiceImpl.NOT_FOUND_USER_BY_EMAIL_VERIFICATION_TOKEN, throwable.getMessage());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(0)).save(any());
        verify(ticketTypeServiceImpl, times(0)).actionAfterCompletedVerificationAccount(any());
        verify(ticketStatusServiceImpl, times(0)).actionAfterCompletedVerificationAccount(any());
    }

    @Test
    void verifyEmailToken_shouldGetJwtException_whenEmailVerificationTokenInDatabaseIsNotEqualPassedToken() {
        String token = jwtProvider.createToken(USER_ID);
        String emailVerificationToken = jwtProvider.createToken(UUID.randomUUID());
        user.setEmailVerificationToken(emailVerificationToken);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findById(any())).thenReturn(Optional.of(user));
        Throwable throwable = assertThrows(JwtException.class, () -> service.verifyEmailToken(token));
        assertEquals(UserServiceImpl.NOT_FOUND_USER_BY_EMAIL_VERIFICATION_TOKEN, throwable.getMessage());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(0)).save(any());
        verify(ticketTypeServiceImpl, times(0)).actionAfterCompletedVerificationAccount(any());
        verify(ticketStatusServiceImpl, times(0)).actionAfterCompletedVerificationAccount(any());
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
        assertEquals(UserServiceImpl.FAILED_SAVE_USER_AFTER_VERIFY_EMAIL_TOKEN, throwable.getMessage());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(1)).save(any());
        verify(ticketTypeServiceImpl, times(0)).actionAfterCompletedVerificationAccount(any());
        verify(ticketStatusServiceImpl, times(0)).actionAfterCompletedVerificationAccount(any());
    }

    @Test
    void verifyEmailToken_shouldGetJwtException_whenTokenIsNull() {
        Throwable throwable = assertThrows(JwtException.class, () -> service.verifyEmailToken(null));
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
    void requestPasswordReset_shouldGetAwsSesException_whenWasErrorWhenCreateEmail()
            throws IOException, MessagingException {
        when(userRepository.getByEmail(any())).thenReturn(Optional.of(user));
        when(senderEmailViaAwsSes.createEmail(any(), any(), any(), any(), any()))
                .thenThrow(new MessagingException(AWS_SES_EXCEPTION));
        var exception = assertThrows(
                AwsSesException.class,
                () -> service.requestPasswordReset(EMAIL_1)
        );
        assertEquals(AWS_SES_EXCEPTION, exception.getMessage());
        verify(userRepository, times(1)).getByEmail(any());
        verify(userRepository, times(1)).save(any());
        verify(senderEmailViaAwsSes, times(1)).createEmail(any(), any(), any(), any(), any());
    }

    @Test
    void requestPasswordReset_shouldGetAwsSesException_whenWasErrorWhenSendEmail() {
        when(userRepository.getByEmail(any())).thenReturn(Optional.of(user));
        when(senderEmailViaAwsSes.sendEmail(any())).thenThrow(new RuntimeException(AWS_SES_EXCEPTION));
        var exception = assertThrows(
                RuntimeException.class,
                () -> service.requestPasswordReset(EMAIL_1)
        );
        assertEquals(AWS_SES_EXCEPTION, exception.getMessage());
        verify(userRepository, times(1)).getByEmail(any());
        verify(userRepository, times(1)).save(any());
        verify(senderEmailViaAwsSes, times(1)).sendEmail(any());
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
        assertEquals(UserServiceImpl.NOT_FOUND_USER_BY_RESET_PASSWORD_TOKEN, throwable.getMessage());
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
        assertEquals(UserServiceImpl.NOT_FOUND_USER_BY_RESET_PASSWORD_TOKEN, throwable.getMessage());
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
        assertEquals(UserServiceImpl.FAILED_SAVE_USER_AFTER_RESET_PASSWORD, throwable.getMessage());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findById(any());
        verify(userRepository, times(1)).save(any());
    }

    @Test
    void requestUpdateEmailOfAccountOwner_shouldUpdateEmailVerificationToken_whenNewEmailIsValid()
            throws IOException, MessagingException {
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(user));
        when(userRepository.getByEmail(any())).thenReturn(Optional.of(user));
        service.requestUpdateEmailOfAccountOwner(EMAIL_1);
        verify(jwtUserBuilder, times(2)).getJwtUser();
        verify(userRepository, times(1)).save(any());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findByIdAndAccountId(any(), any());
        verify(senderEmailViaAwsSes, times(1)).createEmail(any(), any(), any(), any(), any());
        verify(senderEmailViaAwsSes, times(1)).sendEmail(any());
    }

    @Test
    void requestUpdateEmailOfAccountOwner_shouldGetAwsSesException_whenWasErrorWhenCreateEmail()
            throws IOException, MessagingException {
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(user));
        when(userRepository.getByEmail(any())).thenReturn(Optional.of(user));
        when(senderEmailViaAwsSes.createEmail(any(), any(), any(), any(), any()))
                .thenThrow(new MessagingException(AWS_SES_EXCEPTION));
        var exception =
                assertThrows(
                        AwsSesException.class,
                        () -> service.requestUpdateEmailOfAccountOwner(EMAIL_1)
                );
        assertEquals(AWS_SES_EXCEPTION, exception.getMessage());
        verify(jwtUserBuilder, times(2)).getJwtUser();
        verify(userRepository, times(1)).save(any());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findByIdAndAccountId(any(), any());
        verify(senderEmailViaAwsSes, times(1)).createEmail(any(), any(), any(), any(), any());
        verify(senderEmailViaAwsSes, times(0)).sendEmail(any());
    }

    @Test
    void requestUpdateEmailOfAccountOwner_shouldGetAwsSesException_whenWasErrorWhenSendEmail()
            throws IOException, MessagingException {
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(user));
        when(userRepository.getByEmail(any())).thenReturn(Optional.of(user));
        when(senderEmailViaAwsSes.sendEmail(any())).thenThrow(new RuntimeException(AWS_SES_EXCEPTION));
        var exception =
                assertThrows(
                        RuntimeException.class,
                        () -> service.requestUpdateEmailOfAccountOwner(EMAIL_1)
                );
        assertEquals(AWS_SES_EXCEPTION, exception.getMessage());
        verify(jwtUserBuilder, times(2)).getJwtUser();
        verify(userRepository, times(1)).save(any());
        verify(userRepository, times(1)).existsById(any());
        verify(userRepository, times(1)).findByIdAndAccountId(any(), any());
        verify(senderEmailViaAwsSes, times(1)).createEmail(any(), any(), any(), any(), any());
        verify(senderEmailViaAwsSes, times(1)).sendEmail(any());
    }

    @Test
    void updateEmailOfAccountOwner_shouldUpdateEmailOfAccountOwner_whenPassedValidToken() {
        var token = jwtProvider.createToken(EMAIL_1);
        user.setEmailVerificationToken(token);
        when(userRepository.existsById(any())).thenReturn(true);
        when(userRepository.findByIdAndAccountId(any(),any())).thenReturn(Optional.of(user));
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