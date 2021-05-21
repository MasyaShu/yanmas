package ru.itterminal.yanmas.tickets.service.business_handler.ticket;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.tickets.model.Ticket;

@SuppressWarnings("unused")
@Component
@RequiredArgsConstructor
public class SettingGroupFromAuthorOfTicketBeforeCreateAndUpdateTicketBusinessHandler implements EntityBusinessHandler<Ticket> {

    @Override
    public Ticket beforeCreate(Ticket ticket, User currentUser) {
        ticket.setGroup(ticket.getAuthor().getGroup());
        return ticket;
    }

    @Override
    public Ticket beforeUpdate(Ticket ticket, User currentUser) {
        ticket.setGroup(ticket.getAuthor().getGroup());
        return ticket;
    }
}
