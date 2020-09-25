package ru.itterminal.botdesk.aau.service.validator;

import static java.lang.String.format;
import static java.util.Collections.singletonList;
import static ru.itterminal.botdesk.commons.service.CrudService.ENTITY_NOT_EXIST_MESSAGE;
import static ru.itterminal.botdesk.commons.service.CrudService.UPDATE_INIT_MESSAGE;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Component;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.projection.UserUniqueFields;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;

@Slf4j
@Component
public class UserOperationValidator extends BasicOperationValidatorImpl<User> {
    @Autowired
    UserServiceImpl service;

    @Autowired
    BCryptPasswordEncoder encoder;

    @Override
    public boolean checkUniqueness(User entity) {
        log.trace(CHECK_UNIQUENESS, entity);
        Map<String, List<ValidationError>> errors = new HashMap<>();
        List<UserUniqueFields> foundUser = service.findByUniqueFields(entity);
        if (foundUser.isEmpty()) {
            log.trace(FIELDS_UNIQUE, entity);
            return true;
        } else {
            String validatedField;
            if (entity.getEmail().equalsIgnoreCase(foundUser.get(0).getEmail())) {
                validatedField = "email";
                errors.put(validatedField, singletonList(new ValidationError(NOT_UNIQUE_CODE,
                        format(NOT_UNIQUE_MESSAGE, validatedField))));
            }
            log.error(FIELDS_NOT_UNIQUE, errors);
            throw new LogicalValidationException(VALIDATION_FAILED, errors);
        }
    }


    @Override
    public boolean checkLogicalDelete(UUID id) {
        super.checkLogicalDelete(id);
        // TODO chek last user with role SuperAdmin
        return true;
    }
}
