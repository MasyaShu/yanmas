package ru.itterminal.botdesk.tickets.service.validator;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.projection.TicketStatusUniqueFields;
import ru.itterminal.botdesk.tickets.service.impl.TicketStatusServiceImpl;

import java.util.List;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.checkStringForEquals;

@Slf4j
@Component
@RequiredArgsConstructor
public class TicketStatusOperationValidator extends BasicOperationValidatorImpl<TicketStatus> {

    private final TicketStatusServiceImpl service;

    @Override
    public boolean checkUniqueness(TicketStatus entity) {
        log.trace(CHECK_UNIQUENESS, entity);
        List<TicketStatusUniqueFields> foundTicketStatus = service.findByUniqueFields(entity);
        if (!foundTicketStatus.isEmpty()) {
            String validatedField = "name";
            checkStringForEquals(entity.getName(), foundTicketStatus.get(0).getName(),
                    validatedField, format(NOT_UNIQUE_MESSAGE, validatedField));
        }
        log.trace(FIELDS_UNIQUE, entity);
        return true;
    }
}