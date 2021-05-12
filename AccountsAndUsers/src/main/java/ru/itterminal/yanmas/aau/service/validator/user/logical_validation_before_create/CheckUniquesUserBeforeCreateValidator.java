package ru.itterminal.yanmas.aau.service.validator.user.logical_validation_before_create;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.repository.UserRepository;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;

import java.util.List;
import java.util.Map;

import static java.lang.String.format;
import static ru.itterminal.yanmas.aau.service.validator.user.logical_validation_before_update.CheckUniquesUserBeforeUpdateValidator.EMAIL;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

@Component
@RequiredArgsConstructor
public class CheckUniquesUserBeforeCreateValidator implements EntityValidator<User> {

    private final UserRepository repository;

    @Override
    public void logicalValidationBeforeCreate(User entity,
                                              Map<String, List<ValidationError>> errors) {
        var foundUser = repository.getByEmail(entity.getEmail());
        if (foundUser.isPresent()) {
            addValidationErrorIntoErrors(
                    NOT_UNIQUE_CODE,
                    format(NOT_UNIQUE_MESSAGE, EMAIL),
                    errors
            );
        }
    }
}
