package ru.itterminal.yanmas.tickets.service.validator;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.service.validator.BasicOperationValidatorWithCurrentUserImpl;
import ru.itterminal.yanmas.tickets.model.Ticket;

@Component
public class TicketOperationValidator extends BasicOperationValidatorWithCurrentUserImpl<Ticket> {


}
