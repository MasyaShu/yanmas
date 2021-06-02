package ru.itterminal.yanmas.tickets.service.business_handler.ticket;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.service.impl.TicketCounterServiceImpl;
import ru.itterminal.yanmas.tickets.service.impl.TicketServiceImpl;

@Component
@RequiredArgsConstructor
public class SettingTicketNumberBeforeCreateUpdateTicketBusinessHandler implements EntityBusinessHandler<Ticket> {

    private final TicketCounterServiceImpl ticketCounterService;
    private final TicketServiceImpl ticketService;


    @Override
    public Ticket beforeCreate(Ticket ticket, User currentUser) {
        var ticketNumber = ticketCounterService.getNextTicketNumber(currentUser.getAccount().getId());
        ticket.setNumber(ticketNumber);
        return ticket;
    }

    @Override
    public Ticket beforeUpdate(Ticket ticket, User currentUser) {
        var ticketFromDatabase = ticketService.findByIdAndAccountId(ticket.getId(), currentUser);
        ticket.setNumber(ticketFromDatabase.getNumber());
        return ticket;
    }
}
