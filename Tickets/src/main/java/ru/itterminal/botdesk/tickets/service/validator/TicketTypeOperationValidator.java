package ru.itterminal.botdesk.tickets.service.validator;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.projection.TicketTypeUniqueFields;
import ru.itterminal.botdesk.tickets.service.impl.TicketTypeServiceImpl;

import java.util.List;

import static java.lang.String.format;
import static java.util.Collections.singletonList;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;

@Slf4j
@Component
public class TicketTypeOperationValidator extends BasicOperationValidatorImpl<TicketType> {

    private final TicketTypeServiceImpl service;

    @Autowired
    public TicketTypeOperationValidator(TicketTypeServiceImpl service) {
        this.service = service;
    }

    @Override
    public boolean checkUniqueness(TicketType entity) {
        log.trace(CHECK_UNIQUENESS, entity);
        var errors = createMapForLogicalErrors();
        List<TicketTypeUniqueFields> foundTicketTypes = service.findByUniqueFields(entity);
        if (foundTicketTypes.isEmpty()) {
            log.trace(FIELDS_UNIQUE, entity);
            return true;
        } else {
            String validatedField;
            if (entity.getName().equalsIgnoreCase(foundTicketTypes.get(0).getName())) {
                validatedField = "name";
                errors.put(validatedField, singletonList(new ValidationError(NOT_UNIQUE_CODE,
                        format(NOT_UNIQUE_MESSAGE, validatedField))));
            }
            log.error(FIELDS_NOT_UNIQUE, errors);
            throw new LogicalValidationException(VALIDATION_FAILED, errors);
        }
    }

}
