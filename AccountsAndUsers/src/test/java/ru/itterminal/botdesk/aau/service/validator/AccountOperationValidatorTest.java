package ru.itterminal.botdesk.aau.service.validator;

import static java.lang.String.format;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static ru.itterminal.botdesk.aau.model.Roles.ADMIN;
import static ru.itterminal.botdesk.aau.service.validator.AccountOperationValidator.EMAIL_OF_ACCOUNT_OWNER;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.NOT_UNIQUE_CODE;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.NOT_UNIQUE_MESSAGE;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.VALIDATION_FAILED;
import static ru.itterminal.botdesk.config.TestSecurityConfig.ACCOUNT_1_ID;
import static ru.itterminal.botdesk.config.TestSecurityConfig.EMAIL_1;
import static ru.itterminal.botdesk.config.TestSecurityConfig.GROUP_1_ID;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(AccountOperationValidator.class)
class AccountOperationValidatorTest {

    @MockBean
    UserServiceImpl userService;

    @Autowired
    private AccountOperationValidator validator;

    private User user;
    private Account account;
    private Group group;
    private Role roleAdmin = new Role(ADMIN.toString(), ADMIN.getWeight());
    private static String PASSWORD = "12345";
    private static LogicalValidationException logicalValidationException;
    private static Map<String, List<ValidationError>> errors = new HashMap<>();

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
        user.setId(UUID.fromString("aeeeec9e-af15-4b90-b092-881563e808d5"));
    }

    @Test
    public void checkUniqueness_shouldGetTrue_whenPassedValidData() {
        when(userService.findByEmail(any())).thenReturn(Optional.empty());
        assertTrue(validator.checkUniqueness(EMAIL_1));
    }

    @Test
    public void checkUniqueness_shouldGetLogicalValidationException_whenEmailAlreadyExistInDatabase() {
        when(userService.findByEmail(any())).thenReturn(Optional.of(user));
        errors.put(EMAIL_OF_ACCOUNT_OWNER, singletonList(new ValidationError(NOT_UNIQUE_CODE,
                format(NOT_UNIQUE_MESSAGE, EMAIL_1))));
        logicalValidationException = new LogicalValidationException(VALIDATION_FAILED, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.checkUniqueness(EMAIL_1));
        assertEquals(logicalValidationException.getFieldErrors().get(EMAIL_OF_ACCOUNT_OWNER).get(0),
                thrown.getFieldErrors().get(EMAIL_OF_ACCOUNT_OWNER).get(0));
    }

}