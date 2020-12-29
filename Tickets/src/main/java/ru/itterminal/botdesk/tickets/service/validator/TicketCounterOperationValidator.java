package ru.itterminal.botdesk.tickets.service.validator;

import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createExpectedLogicalValidationException;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.tickets.model.TicketCounter;
import ru.itterminal.botdesk.tickets.service.impl.TicketCounterServiceImpl;

@Slf4j
@Component
@RequiredArgsConstructor
public class TicketCounterOperationValidator extends BasicOperationValidatorImpl<TicketCounter> {

    public static final String CURRENT_TICKET_NUMBER = "Current ticket number";
    public static final String NEW_VALUE_MUST_NOT = "New value mustn't less or equals that old one";

    private final TicketCounterServiceImpl service;

    @Override
    public boolean beforeUpdate(TicketCounter entity) {
        super.beforeUpdate(entity);
        var ticketCounterFromDatabase = service.findById(entity.getId());
        if (entity.getCurrentNumber() <= ticketCounterFromDatabase.getCurrentNumber()) {
            throw createExpectedLogicalValidationException(CURRENT_TICKET_NUMBER, NEW_VALUE_MUST_NOT);
        }
        return true;
    }
}
