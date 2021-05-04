package ru.itterminal.yanmas.aau.service.validator.account;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Account;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.commons.exception.EntityNotExistException;
import ru.itterminal.yanmas.commons.exception.LogicalValidationException;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.lang.String.format;
import static java.util.Collections.singletonList;

@Slf4j
@Component
@RequiredArgsConstructor
public class AccountOperationValidator extends BasicOperationValidatorImpl<Account> {

    private final UserServiceImpl userService;

    public static final String EMAIL_OF_ACCOUNT_OWNER = "Email of account owner";

    @SuppressWarnings("SameReturnValue")
    public boolean checkUniqueness(String emailAccountOwner) {
        log.trace(CHECK_UNIQUENESS, EMAIL_OF_ACCOUNT_OWNER);
        try {
            userService.findByEmailForCreateAccount(emailAccountOwner);
            Map<String, List<ValidationError>> errors = new HashMap<>();
            errors.put(EMAIL_OF_ACCOUNT_OWNER, singletonList(new ValidationError(NOT_UNIQUE_CODE,
                    format(NOT_UNIQUE_MESSAGE, emailAccountOwner))));
            throw new LogicalValidationException(VALIDATION_FAILED, errors);
        } catch (EntityNotExistException e) {
            return true;
        }
    }
}
