package ru.itterminal.botdesk.tickets.service.validator;

import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createLogicalValidationException;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.security.jwt.JwtUserBuilder;
import ru.itterminal.botdesk.tickets.model.TicketCounter;
import ru.itterminal.botdesk.tickets.service.impl.TicketCounterServiceImpl;

@Slf4j
@Component
@RequiredArgsConstructor
public class TicketCounterOperationValidator extends BasicOperationValidatorImpl<TicketCounter> {

    public static final String CURRENT_TICKET_NUMBER = "Current ticket number";
    public static final String NEW_VALUE_MUST_NOT = "New value mustn't less or equals that old one";
    public static final String USER_FROM_OUTER_GROUP_CANT_UPDATE_TICKET_COUNTER =
            "User from outer group can't update ticket counter";

    private final TicketCounterServiceImpl service;
    private final JwtUserBuilder jwtUserBuilder;

    @Override
    public void checkAccessBeforeUpdate(TicketCounter entity) {
        var jwtUser = jwtUserBuilder.getJwtUser();
        if (!jwtUser.isInnerGroup()) {
            throw new AccessDeniedException(USER_FROM_OUTER_GROUP_CANT_UPDATE_TICKET_COUNTER);
        }
    }

    @Override
    public boolean beforeUpdate(TicketCounter entity) {
        super.beforeUpdate(entity);
        var ticketCounterFromDatabase = service.findById(entity.getId());
        if (entity.getCurrentNumber() <= ticketCounterFromDatabase.getCurrentNumber()) {
            throw createLogicalValidationException(CURRENT_TICKET_NUMBER, NEW_VALUE_MUST_NOT);
        }
        return true;
    }
}
