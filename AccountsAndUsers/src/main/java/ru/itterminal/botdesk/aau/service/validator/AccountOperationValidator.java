package ru.itterminal.botdesk.aau.service.validator;

import static java.lang.String.format;
import static java.util.Collections.singletonList;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.springframework.stereotype.Component;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.model.Account;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;

@Slf4j
@Component
public class AccountOperationValidator extends BasicOperationValidatorImpl<Account> {

    private final UserServiceImpl userService;

    public static final String EMAIL_OF_ACCOUNT_OWNER = "Email of account owner";

    public AccountOperationValidator(
            UserServiceImpl userService) {
        this.userService = userService;
    }

    @SuppressWarnings("SameReturnValue")
    public boolean checkUniqueness(String emailAccountOwner) {
        log.trace(CHECK_UNIQUENESS, EMAIL_OF_ACCOUNT_OWNER);
        Optional<User> foundedUser = userService.findByEmail(emailAccountOwner);
        if (foundedUser.isPresent()) {
            Map<String, List<ValidationError>> errors = new HashMap<>();
            errors.put(EMAIL_OF_ACCOUNT_OWNER, singletonList(new ValidationError(NOT_UNIQUE_CODE,
                    format(NOT_UNIQUE_MESSAGE, emailAccountOwner))));
            throw new LogicalValidationException(VALIDATION_FAILED, errors);
        }
        return true;
    }
}
