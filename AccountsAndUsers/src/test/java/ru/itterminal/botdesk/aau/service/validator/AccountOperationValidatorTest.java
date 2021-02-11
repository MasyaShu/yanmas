package ru.itterminal.botdesk.aau.service.validator;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.botdesk.aau.model.*;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static java.lang.String.format;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static ru.itterminal.botdesk.aau.service.validator.AccountOperationValidator.EMAIL_OF_ACCOUNT_OWNER;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.*;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(AccountOperationValidator.class)
class AccountOperationValidatorTest {

    @MockBean
    UserServiceImpl userService;

    @Autowired
    private AccountOperationValidator validator;

    private User user;
    private final Role roleAdmin = new Role(Roles.ADMIN.toString(), Roles.ADMIN.getWeight());
    private static final Map<String, List<ValidationError>> errors = new HashMap<>();

    @BeforeEach
    void setUpBeforeEach() {
        Account account = new Account();
        account.setId(UUID.fromString(TestSecurityConfig.ACCOUNT_1_ID));
        Group group = new Group();
        group.setId(UUID.fromString(TestSecurityConfig.INNER_GROUP_ID));
        String PASSWORD = "12345";
        user = User
                .builder()
                .email(TestSecurityConfig.EMAIL_1)
                .password(PASSWORD)
                .account(account)
                .group(group)
                .isArchived(false)
                .role(roleAdmin)
                .build();
        user.setId(UUID.fromString("aeeeec9e-af15-4b90-b092-881563e808d5"));
    }

    @Test
    void checkUniqueness_shouldGetTrue_whenPassedValidData() {
        when(userService.findByEmailForCreateAccount(any())).thenThrow(EntityNotExistException.class);
        assertTrue(validator.checkUniqueness(TestSecurityConfig.EMAIL_1));
    }

    @Test
    void checkUniqueness_shouldGetLogicalValidationException_whenEmailAlreadyExistInDatabase() {
        when(userService.findByEmail(any())).thenReturn(user);
        errors.put(EMAIL_OF_ACCOUNT_OWNER, singletonList(new ValidationError(NOT_UNIQUE_CODE,
                format(NOT_UNIQUE_MESSAGE, TestSecurityConfig.EMAIL_1))));
        LogicalValidationException logicalValidationException =
                new LogicalValidationException(VALIDATION_FAILED, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.checkUniqueness(TestSecurityConfig.EMAIL_1));
        Assertions.assertEquals(logicalValidationException.getFieldErrors().get(EMAIL_OF_ACCOUNT_OWNER).get(0),
                thrown.getFieldErrors().get(EMAIL_OF_ACCOUNT_OWNER).get(0));
    }

}