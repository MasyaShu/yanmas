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
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceWithAccountImpl;
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
        setValueToFieldsOnCreation(entity, currentUser);
        validator.checkUniqueness(entity);
        var createdEntity = repository.create(entity);
        log.trace(format(CREATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), createdEntity.toString()));
        return createdEntity;
    }

    public Ticket update(Ticket entity, User currentUser) {
        log.trace(format(UPDATE_INIT_MESSAGE, entity.getClass().getSimpleName(), entity.getId(),
                entity.toString() + BY_USER + currentUser.toString()
        ));
        validator.beforeUpdate(entity);
        setValueToFieldsOnUpdate(entity, currentUser);
        try {
            entity.generateDisplayName();
            var updatedEntity = repository.update(entity);
            log.trace(format(UPDATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), entity.getId(), updatedEntity));
            return updatedEntity;
        } catch (OptimisticLockException | ObjectOptimisticLockingFailureException ex) {
            throw new OptimisticLockingFailureException(format(VERSION_INVALID_MESSAGE, entity.getId()));
        }
    }

    @SuppressWarnings("UnusedReturnValue")
    public boolean checkAccessForRead(Ticket ticket) {
        return validator.checkAccessForRead(ticket);
    }

    private void setValueToFieldsOnCreation(Ticket entity, User currentUser) {
        UUID id = UUID.randomUUID();
        entity.setId(id);
        entity.generateDisplayName();
        entity.setGroup(entity.getAuthor().getGroup());
        entity.setNumber(ticketCounterService.getTicketNumber(entity.getAccount().getId()));

        if ((currentUser.getRole().getWeight() <= 40 && !currentUser.getGroup().getIsInner())
                || currentUser.getRole().getWeight() <= 20) {
            var ticketSetting = ticketSettingService.getSettingOrPredefinedValuesForTicket
                    (entity.getAccount().getId(), entity.getGroup().getId(),
                            entity.getAuthor().getId());
            entity.setTicketStatus(ticketSetting.getTicketStatusForNew());
            entity.setTicketType(ticketSetting.getTicketTypeForNew());
            entity.setExecutors(ticketSetting.getExecutors());
            entity.setObservers(ticketSetting.getObservers());
        }
    }

    private void setValueToFieldsOnUpdate(Ticket entity, User currentUser) {
        var entityFromDatabase = super.findByIdAndAccountId(entity.getId(), entity.getAccount().getId());
        entity.setNumber(entityFromDatabase.getNumber());
        entity.setCreatedAt(entityFromDatabase.getCreatedAt());
        entity.setFiles(entityFromDatabase.getFiles());
        entity.setTicketTemplate(entityFromDatabase.getTicketTemplate());
        entity.setGroup(entity.getAuthor().getGroup());

        if ((currentUser.getRole().getWeight() <= 40 && !currentUser.getGroup().getIsInner())
                || currentUser.getRole().getWeight() <= 20) {
            entity.setExecutors(entityFromDatabase.getExecutors());
            entity.setObservers(entityFromDatabase.getObservers());
        }
    }

}
