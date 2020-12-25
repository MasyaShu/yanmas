package ru.itterminal.botdesk.tickets.service.validator;

import org.springframework.stereotype.Component;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.tickets.model.TicketCounter;

@Slf4j
@Component
public class TicketCounterOperationValidator extends BasicOperationValidatorImpl<TicketCounter> {

}
