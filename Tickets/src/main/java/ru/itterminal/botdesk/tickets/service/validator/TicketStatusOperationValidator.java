package ru.itterminal.botdesk.tickets.service.validator;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.projection.TicketStatusUniqueFields;
import ru.itterminal.botdesk.tickets.service.impl.TicketStatusServiceImpl;

import java.util.List;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekStringForEquals;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;

@Slf4j
@Component
public class TicketStatusOperationValidator extends BasicOperationValidatorImpl<TicketStatus> {

    private final TicketStatusServiceImpl service;

    @Autowired
    public TicketStatusOperationValidator(TicketStatusServiceImpl service) {
        this.service = service;
    }

    @Override
    public boolean checkUniqueness(TicketStatus entity) {
        log.trace(CHECK_UNIQUENESS, entity);
        var errors = createMapForLogicalErrors();
        List<TicketStatusUniqueFields> foundTicketStatus = service.findByUniqueFields(entity);
        if (!foundTicketStatus.isEmpty()) {
            chekStringForEquals(entity.getName(), foundTicketStatus.get(0).getName(),
                    NOT_UNIQUE_CODE, format(NOT_UNIQUE_MESSAGE, "name"), errors);
        }
        log.trace(FIELDS_UNIQUE, entity);
        return true;
    }
}