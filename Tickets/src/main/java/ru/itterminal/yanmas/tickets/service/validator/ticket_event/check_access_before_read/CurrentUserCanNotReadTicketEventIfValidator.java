package ru.itterminal.yanmas.tickets.service.validator.ticket_event.check_access_before_read;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.tickets.model.TicketEvent;
import ru.itterminal.yanmas.tickets.service.impl.TicketServiceImpl;

@Component
@RequiredArgsConstructor
public class CurrentUserCanNotReadTicketEventIfValidator implements EntityValidator<TicketEvent> {
    private final TicketServiceImpl ticketService;

    @Override
    public void checkAccessBeforeRead(TicketEvent entity, User currentUser) {
        ticketService.findByIdAndAccountId(entity.getTicket().getId(), currentUser);
    }
}
