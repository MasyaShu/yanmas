package ru.itterminal.yanmas.tickets.service.business_handler.ticket;

import java.util.stream.Collectors;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.files.model.File;
import ru.itterminal.yanmas.files.service.FileServiceImpl;
import ru.itterminal.yanmas.tickets.model.Priority;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.service.impl.TicketServiceImpl;
import ru.itterminal.yanmas.tickets.service.impl.TicketSettingServiceImpl;
import ru.itterminal.yanmas.tickets.service.impl.TicketStatusServiceImpl;
import ru.itterminal.yanmas.tickets.service.impl.TicketTypeServiceImpl;

@Component
@RequiredArgsConstructor
public class TicketBusinessHandler implements EntityBusinessHandler<Ticket> {

    private final FileServiceImpl fileService;
    private final TicketSettingServiceImpl ticketSettingService;
    private final TicketTypeServiceImpl ticketTypeService;
    private final TicketStatusServiceImpl ticketStatusService;
    private final TicketServiceImpl ticketService;
    private final UserServiceImpl userService;

    @Override
    public void afterCreate(Ticket createdTicket, User currentUser) {
        if (createdTicket.getFiles() != null && !createdTicket.getFiles().isEmpty()) {
            for (File file : createdTicket.getFiles()) {
                file.setEntityId(createdTicket.getId());
                fileService.update(file, currentUser);
            }
        }
    }

    @Override
    public Ticket beforeCreate(Ticket ticket, User currentUser) {
        ticket.setGroup(ticket.getAuthor().getGroup());

        var ticketSetting = ticketSettingService.getSettingOrPredefinedValuesForTicket(
                currentUser,
                ticket.getAuthor()
        );
        boolean isCurrentUserFromInnerGroup = currentUser.getGroup().getIsInner();
        var isTicketFinished = ticket.getIsFinished();
        var ticketStatus = ticket.getTicketStatus();
        var weightOfRoleOfCurrentUser = currentUser.getRole().getWeight();

        // SettingTicketStatusBeforeCreateAndUpdateTicketBusinessHandler
        // ticket.status
        if (Boolean.TRUE.equals(isTicketFinished) && (weightOfRoleOfCurrentUser >= Roles.EXECUTOR.getWeight())) {
            ticket.setTicketStatus(ticketSetting.getTicketStatusForClose());
        } else if (ticketStatus != null && Boolean.FALSE.equals(isTicketFinished) && isCurrentUserFromInnerGroup
                && (weightOfRoleOfCurrentUser >= Roles.EXECUTOR.getWeight())) {
            ticket.setTicketStatus(ticketStatusService.findByIdAndAccountId(ticketStatus.getId(), currentUser));
        } else if ((Boolean.FALSE.equals(isTicketFinished) && ticket.getTicketStatus() == null)
                || (Boolean.FALSE.equals(isTicketFinished) && !isCurrentUserFromInnerGroup)
                || (Boolean.FALSE.equals(isTicketFinished) && weightOfRoleOfCurrentUser == Roles.AUTHOR.getWeight())) {
            ticket.setTicketStatus(ticketSetting.getTicketStatusForNew());
        }

        // SettingTicketTypeBeforeCreateAndUpdateTicketBusinessHandler
        // ticket.ticketType
        var ticketType = ticket.getTicketType();
        if (ticketType == null || !isCurrentUserFromInnerGroup
                || weightOfRoleOfCurrentUser == Roles.AUTHOR.getWeight()) {
            ticket.setTicketType(ticketSetting.getTicketTypeForNew());
        } else if (weightOfRoleOfCurrentUser >= Roles.EXECUTOR.getWeight()) {
            ticket.setTicketType(ticketTypeService.findByIdAndAccountId(ticketType.getId(), currentUser));
        }

        // SettingObserversBeforeCreateAndUpdateTicketBusinessHandler
        // ticket.observers
        if (ticket.getObservers() == null || !isCurrentUserFromInnerGroup
                || weightOfRoleOfCurrentUser == Roles.AUTHOR.getWeight()) {
            ticket.setObservers(ticketSetting.getObservers());
        } else if (weightOfRoleOfCurrentUser >= Roles.EXECUTOR.getWeight()) {
            var listObserversId = ticket.getObservers().stream()
                    .map(BaseEntity::getId)
                    .collect(Collectors.toList());
            ticket.setObservers(userService.findAllByAccountIdAndListId(listObserversId, currentUser));
        }

        // SettingTicketExecutorsBeforeCreateAndUpdateTicketBusinessHandler
        // ticket.executors
        if (ticket.getExecutors() == null || !isCurrentUserFromInnerGroup
                || weightOfRoleOfCurrentUser == Roles.AUTHOR.getWeight()) {
            ticket.setExecutors(ticketSetting.getExecutors());
        } else if (weightOfRoleOfCurrentUser >= Roles.EXECUTOR.getWeight()) {
            var listExecutorsId = ticket.getExecutors().stream()
                    .map(BaseEntity::getId)
                    .collect(Collectors.toList());
            ticket.setExecutors(userService.findAllByAccountIdAndListId(listExecutorsId, currentUser));
        }

        // SettingTicketPriorityBeforeCreateAndUpdateTicketBusinessHandler
        // ticket.priority
        if (ticket.getPriority() == null) {
            ticket.setPriority(Priority.MIDDLE.toString());
        }

        return ticket;
    }

    @Override
    public Ticket beforeUpdate(Ticket ticket, User currentUser) {
        ticket.setGroup(ticket.getAuthor().getGroup());
        var ticketFromDatabase = ticketService.findByIdAndAccountId(ticket.getId(), currentUser);
        ticket.setTicketTemplate(ticketFromDatabase.getTicketTemplate());
        ticket.setNumber(ticketFromDatabase.getNumber());
        ticket.setCreatedAt(ticketFromDatabase.getCreatedAt());
        ticket.setFiles(ticketFromDatabase.getFiles());
        ticket.setAccount(ticketFromDatabase.getAccount());
        var ticketSetting = ticketSettingService.getSettingOrPredefinedValuesForTicket(
                currentUser,
                ticket.getAuthor()
        );
        boolean isCurrentUserFromInnerGroup = currentUser.getGroup().getIsInner();
        var isTicketFinished = ticket.getIsFinished();
        var isTicketFinishedBeforeUpdate = ticketFromDatabase.getIsFinished();
        var weightOfRoleOfCurrentUser = currentUser.getRole().getWeight();

        // ticket.status
        var ticketStatus = ticket.getTicketStatus();
        if (Boolean.TRUE.equals(isTicketFinished) && Boolean.FALSE.equals(isTicketFinishedBeforeUpdate)) {
            ticket.setTicketStatus(ticketSetting.getTicketStatusForClose());
        } else if (ticketStatus == null) {
            ticket.setTicketStatus(ticketFromDatabase.getTicketStatus());
        } else if (isTicketFinished == null || !isTicketFinished) {
            ticket.setTicketStatus(ticketStatusService.findByIdAndAccountId(ticketStatus.getId(), currentUser));
        }

        // ticket.ticketType
        var ticketType = ticket.getTicketType();
        if (ticketType == null || !isCurrentUserFromInnerGroup
                || weightOfRoleOfCurrentUser == Roles.AUTHOR.getWeight()) {
            ticket.setTicketType(ticketFromDatabase.getTicketType());
        } else if (weightOfRoleOfCurrentUser >= Roles.EXECUTOR.getWeight()) {
            ticket.setTicketType(ticketTypeService.findByIdAndAccountId(ticketType.getId(), currentUser));
        }

        // ticket.observers
        if (!isCurrentUserFromInnerGroup || weightOfRoleOfCurrentUser == Roles.AUTHOR.getWeight()) {
            ticket.setObservers(ticketFromDatabase.getObservers());
        } else if (weightOfRoleOfCurrentUser >= Roles.EXECUTOR.getWeight()) {
            var listObserversId = ticket.getObservers().stream()
                    .map(BaseEntity::getId)
                    .collect(Collectors.toList());
            ticket.setObservers(userService.findAllByAccountIdAndListId(listObserversId, currentUser));
        }
        // ticket.executors
        if (!isCurrentUserFromInnerGroup || weightOfRoleOfCurrentUser == Roles.AUTHOR.getWeight()) {
            ticket.setExecutors(ticketFromDatabase.getExecutors());
        } else if (weightOfRoleOfCurrentUser >= Roles.EXECUTOR.getWeight()) {
            var listExecutorsId = ticket.getExecutors().stream()
                    .map(BaseEntity::getId)
                    .collect(Collectors.toList());
            ticket.setExecutors(userService.findAllByAccountIdAndListId(listExecutorsId, currentUser));
        }

        // ticket.priority
        if (ticket.getPriority() == null) {
            ticket.setPriority(ticketFromDatabase.getPriority());
        }

        return ticket;
    }
}
