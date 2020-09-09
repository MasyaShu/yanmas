package ru.itterminal.botdesk.aau.service.validator;

import static java.lang.String.format;
import static java.util.Collections.singletonList;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.projection.UserUniqueFields;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;

@Slf4j
@Component
public class UserOperationValidator extends BasicOperationValidatorImpl<User> {
    @Autowired
    UserServiceImpl service;

    @Override
    public boolean checkUniqueness(User entity) {
        log.trace(CHECK_UNIQUENESS, entity);
        Map<String, List<ValidationError>> errors = new HashMap<>();
        UserUniqueFields foundUser = service.findByUniqueFields(entity);
        if (foundUser.getEmail().isEmpty()) {
            log.trace(FIELDS_UNIQUE, entity);
            return true;
        } else {
                String validatedField;
                if (entity.getEmail().equalsIgnoreCase(foundUser.getEmail())) {
                    validatedField = "email";
                    errors.put(validatedField, singletonList(new ValidationError(NOT_UNIQUE_CODE,
                            format(NOT_UNIQUE_MESSAGE, validatedField))));
                }
            log.error(FIELDS_NOT_UNIQUE, errors);
            throw new LogicalValidationException(VALIDATION_FAILED, errors);
        }
    }

//    @Override
//    public boolean checkBeforeCreate(User entity) {
//        super.checkBeforeCreate(entity);
//        if (entity.getRoles().isEmpty()) {
//
//        }
//        return true;
//    }
//
//    @Override
//    public boolean checkBeforeUpdate(User entity) {
//        super.checkBeforeUpdate(entity);
//        return true;
//    }

    @Override
    public boolean checkLogicalDelete(UUID id) {
        super.checkLogicalDelete(id);
        // TODO chek last user with role SuperAdmin
        return true;
    }
}
