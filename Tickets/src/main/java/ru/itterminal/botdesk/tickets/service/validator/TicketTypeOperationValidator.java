package ru.itterminal.botdesk.tickets.service.validator;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.projection.TicketTypeUniqueFields;
import ru.itterminal.botdesk.tickets.service.impl.TicketTypeServiceImpl;

import java.util.List;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.checkStringForEquals;

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
        List<TicketTypeUniqueFields> foundTicketTypes = service.findByUniqueFields(entity);
        if (!foundTicketTypes.isEmpty()) {
            String validatedField = "name";
            checkStringForEquals(entity.getName(), foundTicketTypes.get(0).getName(),
                    validatedField, format(NOT_UNIQUE_MESSAGE, validatedField));
        }
        log.trace(FIELDS_UNIQUE, entity);
        return true;
    }
}
