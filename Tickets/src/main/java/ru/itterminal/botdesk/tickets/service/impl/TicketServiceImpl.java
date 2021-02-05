package ru.itterminal.botdesk.tickets.service.impl;

import static java.lang.String.format;

import java.util.UUID;

import javax.persistence.OptimisticLockException;

import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.files.model.File;
import ru.itterminal.botdesk.files.service.FileServiceImpl;
import ru.itterminal.botdesk.tickets.model.Ticket;
import ru.itterminal.botdesk.tickets.repository.TicketRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator;

@SuppressWarnings("unused")
@Slf4j
@Service
@RequiredArgsConstructor
public class TicketServiceImpl extends CrudServiceWithAccountImpl<Ticket, TicketOperationValidator, TicketRepository> {

    public static final String YOU_MUST_USE_ANOTHER_METHOD_CREATE =
            "You must use method create(Ticket entity, User currentUser)";
    public static final String YOU_MUST_USE_ANOTHER_METHOD_UPDATE =
            "You must use method update(Ticket entity, User currentUser)";
    public static final String BY_USER = " by user: ";

    private final TicketCounterServiceImpl ticketCounterService;
    private final TicketSettingServiceImpl ticketSettingService;
    private final FileServiceImpl fileService;

    @Override
    @Deprecated
    public Ticket create(Ticket entity) {
        throw new UnsupportedOperationException(YOU_MUST_USE_ANOTHER_METHOD_CREATE);
    }

    @Override
    @Deprecated
    public Ticket update(Ticket entity) {
        throw new UnsupportedOperationException(YOU_MUST_USE_ANOTHER_METHOD_UPDATE);
    }

    @Transactional
    public Ticket create(Ticket entity, User currentUser) {
        validator.beforeCreate(entity);
        log.trace(format(CREATE_INIT_MESSAGE, entity.getClass().getSimpleName(),
                         entity.toString() + BY_USER + currentUser.getEmail()
        ));
        setPropertiesOfTicketFromSettingsBeforeCreate(entity, currentUser);
        validator.checkUniqueness(entity);
        var createdEntity = repository.create(entity);
        if (createdEntity.getFiles() != null && !createdEntity.getFiles().isEmpty()) {
            for (File file : createdEntity.getFiles()) {
                file.setEntityId(entity.getId());
                fileService.update(file);
            }
        }
        log.trace(format(CREATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), createdEntity.toString()));
        return createdEntity;
    }

    public Ticket update(Ticket entity, User currentUser) {
        log.trace(format(UPDATE_INIT_MESSAGE, entity.getClass().getSimpleName(), entity.getId(),
                         entity.toString() + BY_USER + currentUser.toString()
        ));
        validator.beforeUpdate(entity);
        setPropertiesOfTicketFromDatabaseBeforeUpdate(entity, currentUser);
        try {
            entity.generateDisplayName();
            var updatedEntity = repository.update(entity);
            log.trace(format(UPDATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), entity.getId(), updatedEntity));
            return updatedEntity;
        }
        catch (OptimisticLockException | ObjectOptimisticLockingFailureException ex) {
            throw new OptimisticLockingFailureException(format(VERSION_INVALID_MESSAGE, entity.getId()));
        }
    }

    @SuppressWarnings("UnusedReturnValue")
    public boolean checkAccessForRead(Ticket ticket) {
        return validator.checkAccessForRead(ticket);
    }

    private void setPropertiesOfTicketFromSettingsBeforeCreate(Ticket ticket, User currentUser) {
        ticket.setId(UUID.randomUUID());
        ticket.generateDisplayName();
        ticket.setGroup(ticket.getAuthor().getGroup());
        ticket.setNumber(ticketCounterService.getTicketNumber(ticket.getAccount().getId()));
        if ((currentUser.getRole().getWeight() <= Roles.ADMIN.getWeight() && !currentUser.getGroup().getIsInner())
                || currentUser.getRole().getWeight() <= Roles.AUTHOR.getWeight()) {
            var ticketSetting = ticketSettingService.getSettingOrPredefinedValuesForTicket(
                    ticket.getAccount().getId(), ticket.getGroup().getId(), ticket.getAuthor().getId()
            );
            ticket.setTicketStatus(ticketSetting.getTicketStatusForNew());
            ticket.setTicketType(ticketSetting.getTicketTypeForNew());
            ticket.setExecutors(ticketSetting.getExecutors());
            ticket.setObservers(ticketSetting.getObservers());
        }
    }

    private void setPropertiesOfTicketFromDatabaseBeforeUpdate(Ticket ticket, User currentUser) {
        var ticketFromDatabase = super.findByIdAndAccountId(ticket.getId(), ticket.getAccount().getId());
        ticket.setNumber(ticketFromDatabase.getNumber());
        ticket.setCreatedAt(ticketFromDatabase.getCreatedAt());
        ticket.setFiles(ticketFromDatabase.getFiles());
        ticket.setTicketTemplate(ticketFromDatabase.getTicketTemplate());
        ticket.setGroup(ticket.getAuthor().getGroup());
        if ((currentUser.getRole().getWeight() <= Roles.ADMIN.getWeight() && !currentUser.getGroup().getIsInner())
                || currentUser.getRole().getWeight() <= Roles.AUTHOR.getWeight()) {
            ticket.setExecutors(ticketFromDatabase.getExecutors());
            ticket.setObservers(ticketFromDatabase.getObservers());
        }
    }

}
