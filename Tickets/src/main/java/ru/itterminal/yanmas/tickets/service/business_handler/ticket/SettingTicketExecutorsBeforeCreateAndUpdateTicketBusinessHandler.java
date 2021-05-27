package ru.itterminal.yanmas.tickets.service.business_handler.ticket;

import java.util.ArrayList;
import java.util.stream.Collectors;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.service.impl.TicketServiceImpl;
import ru.itterminal.yanmas.tickets.service.impl.TicketSettingServiceImpl;

@SuppressWarnings("DuplicatedCode")
@Component
@RequiredArgsConstructor
public class SettingTicketExecutorsBeforeCreateAndUpdateTicketBusinessHandler implements EntityBusinessHandler<Ticket> {

    private final TicketSettingServiceImpl ticketSettingService;
    private final TicketServiceImpl ticketService;
    private final UserServiceImpl userService;


    @Override
    public Ticket beforeCreate(Ticket ticket, User currentUser) {
        var ticketSetting = ticketSettingService.getSettingOrPredefinedValuesForTicket(
                currentUser,
                ticket.getAuthor()
        );
        boolean isCurrentUserFromInnerGroup = currentUser.getGroup().getIsInner();
        var weightOfRoleOfCurrentUser = currentUser.getRole().getWeight();
        if (ticket.getExecutors() == null || !isCurrentUserFromInnerGroup
                || weightOfRoleOfCurrentUser == Roles.AUTHOR.getWeight()) {
            ticket.setExecutors(new ArrayList<>(ticketSetting.getExecutors()));
        } else if (weightOfRoleOfCurrentUser >= Roles.EXECUTOR.getWeight()) {
            var listExecutorsId = ticket.getExecutors().stream()
                    .map(BaseEntity::getId)
                    .collect(Collectors.toList());
            ticket.setExecutors(userService.findAllByAccountIdAndListId(listExecutorsId, currentUser));
        }
        return ticket;
    }

    @Override
    public Ticket beforeUpdate(Ticket ticket, User currentUser) {
        var ticketFromDatabase = ticketService.findByIdAndAccountId(ticket.getId(), currentUser);
        boolean isCurrentUserFromInnerGroup = currentUser.getGroup().getIsInner();
        var weightOfRoleOfCurrentUser = currentUser.getRole().getWeight();
        if (!isCurrentUserFromInnerGroup || weightOfRoleOfCurrentUser == Roles.AUTHOR.getWeight()) {
            ticket.setExecutors(ticketFromDatabase.getExecutors());
        } else if (weightOfRoleOfCurrentUser >= Roles.EXECUTOR.getWeight()) {
            var listExecutorsId = ticket.getExecutors().stream()
                    .map(BaseEntity::getId)
                    .collect(Collectors.toList());
            ticket.setExecutors(userService.findAllByAccountIdAndListId(listExecutorsId, currentUser));
        }
        return ticket;
    }
}