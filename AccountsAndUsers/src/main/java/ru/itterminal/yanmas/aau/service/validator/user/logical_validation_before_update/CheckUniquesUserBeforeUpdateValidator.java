package ru.itterminal.yanmas.aau.service.validator.user.logical_validation_before_update;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.repository.UserRepository;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;

import java.util.List;
import java.util.Map;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

@Component
@RequiredArgsConstructor
public class CheckUniquesUserBeforeUpdateValidator implements EntityValidator<User> {

    public static final String EMAIL = "email";
    private final UserRepository repository;

    @Override
    public void logicalValidationBeforeUpdate(User entity,
                                              Map<String, List<ValidationError>> errors) {
        var foundUser = repository.getByEmailAndIdNot(entity.getEmail(), entity.getId());
        if (!foundUser.isEmpty()) {
            addValidationErrorIntoErrors(
                    NOT_UNIQUE_CODE,
                    format(NOT_UNIQUE_MESSAGE, EMAIL),
                    errors
            );
        }
    }
}
