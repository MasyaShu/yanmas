package ru.itterminal.yanmas.tickets.service.business_handler.ticket;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.service.impl.TicketServiceImpl;
import ru.itterminal.yanmas.tickets.service.impl.TicketSettingServiceImpl;
import ru.itterminal.yanmas.tickets.service.impl.TicketStatusServiceImpl;

@SuppressWarnings("DuplicatedCode")
@Component
@RequiredArgsConstructor
public class SettingTicketStatusBeforeCreateAndUpdateTicketBusinessHandler implements EntityBusinessHandler<Ticket> {

    private final TicketSettingServiceImpl ticketSettingService;
    private final TicketStatusServiceImpl ticketStatusService;
    private final TicketServiceImpl ticketService;


    @Override
    public Ticket beforeCreate(Ticket ticket, User currentUser) {
        var ticketSetting = ticketSettingService.getSettingOrPredefinedValuesForTicket(
                currentUser,
                ticket.getAuthor()
        );
        var isTicketFinished = ticket.getIsFinished() != null && Boolean.TRUE.equals(ticket.getIsFinished());
        var ticketStatus = ticket.getTicketStatus();
        var weightOfRoleOfCurrentUser = currentUser.getRole().getWeight();
        if (isTicketFinished && (weightOfRoleOfCurrentUser >= Roles.EXECUTOR.getWeight())) {
            ticket.setTicketStatus(ticketSetting.getTicketStatusForClose());
        } else if (ticketStatus != null && !isTicketFinished
                && (weightOfRoleOfCurrentUser >= Roles.EXECUTOR.getWeight())) {
            ticket.setTicketStatus(ticketStatusService.findByIdAndAccountId(ticketStatus.getId(), currentUser));
        } else if (
                (ticket.getTicketStatus() == null)
                        || (weightOfRoleOfCurrentUser == Roles.AUTHOR.getWeight())
        ) {
            ticket.setTicketStatus(ticketSetting.getTicketStatusForNew());
            ticket.setIsFinished(false);
        }
        return ticket;
    }

    @Override
    public Ticket beforeUpdate(Ticket ticket, User currentUser) {
        var ticketFromDatabase = ticketService.findByIdAndAccountId(ticket.getId(), currentUser);
        var ticketSetting = ticketSettingService.getSettingOrPredefinedValuesForTicket(
                currentUser,
                ticket.getAuthor()
        );
        var isTicketFinished = ticket.getIsFinished();
        var isTicketFinishedBeforeUpdate = ticketFromDatabase.getIsFinished();
        var ticketStatus = ticket.getTicketStatus();
        if (Boolean.TRUE.equals(isTicketFinished) && Boolean.FALSE.equals(isTicketFinishedBeforeUpdate)) {
            ticket.setTicketStatus(ticketSetting.getTicketStatusForClose());
        } else if (ticketStatus == null) {
            ticket.setTicketStatus(ticketFromDatabase.getTicketStatus());
        } else if (isTicketFinished == null || !isTicketFinished) {
            ticket.setTicketStatus(ticketStatusService.findByIdAndAccountId(ticketStatus.getId(), currentUser));
        }
        return ticket;
    }
}
