package ru.itterminal.yanmas.tickets.service.business_handler.ticket;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.service.impl.TicketServiceImpl;

@SuppressWarnings("DuplicatedCode")
@Component
@RequiredArgsConstructor
public class SettingFilesFromDataBaseBeforeUpdateTicketBusinessHandler implements EntityBusinessHandler<Ticket> {

    private final TicketServiceImpl ticketService;

    @Override
    public Ticket beforeUpdate(Ticket ticket, User currentUser) {
        var ticketFromDatabase = ticketService.findByIdAndAccountId(ticket.getId(), currentUser);
        ticket.setFiles(ticketFromDatabase.getFiles());
        return ticket;
    }
}
