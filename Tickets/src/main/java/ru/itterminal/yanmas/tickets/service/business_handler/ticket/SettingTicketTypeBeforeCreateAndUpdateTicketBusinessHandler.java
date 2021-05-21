package ru.itterminal.yanmas.tickets.service.business_handler.ticket;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.service.impl.TicketServiceImpl;
import ru.itterminal.yanmas.tickets.service.impl.TicketSettingServiceImpl;
import ru.itterminal.yanmas.tickets.service.impl.TicketTypeServiceImpl;

@Component
@RequiredArgsConstructor
public class SettingTicketTypeBeforeCreateAndUpdateTicketBusinessHandler implements EntityBusinessHandler<Ticket> {

    private final TicketSettingServiceImpl ticketSettingService;
    private final TicketTypeServiceImpl ticketTypeService;
    private final TicketServiceImpl ticketService;

    @Override
    public Ticket beforeCreate(Ticket ticket, User currentUser) {
        var ticketSetting = ticketSettingService.getSettingOrPredefinedValuesForTicket(
                currentUser,
                ticket.getAuthor()
        );
        boolean isCurrentUserFromInnerGroup = currentUser.getGroup().getIsInner();
        var weightOfRoleOfCurrentUser = currentUser.getRole().getWeight();
        var ticketType = ticket.getTicketType();
        if (ticketType == null || !isCurrentUserFromInnerGroup
                || weightOfRoleOfCurrentUser == Roles.AUTHOR.getWeight()) {
            ticket.setTicketType(ticketSetting.getTicketTypeForNew());
        } else if (weightOfRoleOfCurrentUser >= Roles.EXECUTOR.getWeight()) {
            ticket.setTicketType(ticketTypeService.findByIdAndAccountId(ticketType.getId(), currentUser));
        }
        return ticket;
    }

    @Override
    public Ticket beforeUpdate(Ticket ticket, User currentUser) {
        var ticketFromDatabase = ticketService.findByIdAndAccountId(ticket.getId(), currentUser);
        boolean isCurrentUserFromInnerGroup = currentUser.getGroup().getIsInner();
        var weightOfRoleOfCurrentUser = currentUser.getRole().getWeight();
        var ticketType = ticket.getTicketType();
        if (ticketType == null || !isCurrentUserFromInnerGroup
                || weightOfRoleOfCurrentUser == Roles.AUTHOR.getWeight()) {
            ticket.setTicketType(ticketFromDatabase.getTicketType());
        } else if (weightOfRoleOfCurrentUser >= Roles.EXECUTOR.getWeight()) {
            ticket.setTicketType(ticketTypeService.findByIdAndAccountId(ticketType.getId(), currentUser));
        }
        return ticket;
    }
}
