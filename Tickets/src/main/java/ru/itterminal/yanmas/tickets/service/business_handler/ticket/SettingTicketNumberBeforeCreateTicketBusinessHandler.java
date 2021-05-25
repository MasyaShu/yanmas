package ru.itterminal.yanmas.tickets.service.business_handler.ticket;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.service.impl.TicketCounterServiceImpl;

@Component
@RequiredArgsConstructor
public class SettingTicketNumberBeforeCreateTicketBusinessHandler implements EntityBusinessHandler<Ticket> {

    private final TicketCounterServiceImpl ticketCounterService;

    @Override
    public Ticket beforeCreate(Ticket ticket, User currentUser) {
        var ticketNumber = ticketCounterService.getNextTicketNumber(currentUser.getAccount().getId());
        ticket.setNumber(ticketNumber);
        return ticket;
    }
}
