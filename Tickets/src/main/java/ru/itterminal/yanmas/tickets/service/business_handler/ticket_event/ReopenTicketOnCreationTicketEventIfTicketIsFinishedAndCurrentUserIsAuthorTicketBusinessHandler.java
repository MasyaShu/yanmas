package ru.itterminal.yanmas.tickets.service.business_handler.ticket_event;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.tickets.model.TicketEvent;
import ru.itterminal.yanmas.tickets.service.impl.TicketServiceImpl;

import static java.lang.String.format;

@Component
@RequiredArgsConstructor
public class ReopenTicketOnCreationTicketEventIfTicketIsFinishedAndCurrentUserIsAuthorTicketBusinessHandler implements EntityBusinessHandler<TicketEvent> {

    public static final String USER_REOPEN_TICKET = "User %s reopen ticket";
    private final TicketServiceImpl ticketService;

     @Override
    public TicketEvent beforeCreate(TicketEvent entity, User currentUser) {
        var ticket = ticketService.findByIdAndAccountId(entity.getTicket().getId(), currentUser);
        if (Boolean.TRUE.equals(ticket.getIsFinished()) && ticket.getAuthor().equals(currentUser)) {
            entity.setAutoComment(format(USER_REOPEN_TICKET, currentUser.getDisplayName()));
        }
        return entity;
    }

    @Override
    public void afterCreate(TicketEvent entity, User currentUser) {
        var ticket = ticketService.findByIdAndAccountId(entity.getTicket().getId(), currentUser);
        if (Boolean.TRUE.equals(ticket.getIsFinished()) && ticket.getAuthor().equals(currentUser)) {
            ticketService.reOpen(ticket, currentUser);
        }
    }
}
