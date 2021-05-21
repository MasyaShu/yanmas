package ru.itterminal.yanmas.tickets.service.business_handler.ticket;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.tickets.model.Priority;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.service.impl.TicketServiceImpl;

@Component
@RequiredArgsConstructor
public class SettingTicketPriorityBeforeCreateAndUpdateTicketBusinessHandler implements EntityBusinessHandler<Ticket> {

    private final TicketServiceImpl ticketService;

    @Override
    public Ticket beforeCreate(Ticket ticket, User currentUser) {
        if (ticket.getPriority() == null) {
            ticket.setPriority(Priority.MIDDLE.toString());
        }
        return ticket;
    }

    @Override
    public Ticket beforeUpdate(Ticket ticket, User currentUser) {
        var ticketFromDatabase = ticketService.findByIdAndAccountId(ticket.getId(), currentUser);
        if (ticket.getPriority() == null) {
            ticket.setPriority(ticketFromDatabase.getPriority());
        }
        return ticket;
    }
}
