package ru.itterminal.botdesk.tickets.service.impl;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter;
import ru.itterminal.botdesk.commons.model.filter.ListOfBaseEntityFilter;
import ru.itterminal.botdesk.commons.model.spec.SpecificationsFactory;
import ru.itterminal.botdesk.files.model.File;
import ru.itterminal.botdesk.files.service.FileServiceImpl;
import ru.itterminal.botdesk.integration.across_modules.RequestsFromModuleAccountAndUsers;
import ru.itterminal.botdesk.tickets.model.Ticket;
import ru.itterminal.botdesk.tickets.repository.TicketRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator;

import javax.persistence.OptimisticLockException;
import java.util.List;
import java.util.UUID;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;
import static ru.itterminal.botdesk.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.CONTAINS_ALL_OF_LIST;

@SuppressWarnings("unused")
@Slf4j
@Service
@RequiredArgsConstructor
public class TicketServiceImpl extends CrudServiceWithAccountImpl<Ticket, TicketOperationValidator, TicketRepository> implements RequestsFromModuleAccountAndUsers {

    public static final String YOU_MUST_USE_ANOTHER_METHOD_CREATE =
            "You must use method create(Ticket entity, User currentUser)";
    public static final String YOU_MUST_USE_ANOTHER_METHOD_UPDATE =
            "You must use method update(Ticket entity, User currentUser)";
    public static final String BY_USER = " by user: ";
    public static final String AUTHOR = "author";
    public static final String OBSERVERS = "observers";
    public static final String EXECUTORS = "executors";

    private final TicketCounterServiceImpl ticketCounterService;
    private final TicketSettingServiceImpl ticketSettingService;
    private final FileServiceImpl fileService;
    private final SpecificationsFactory specFactory;


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
        var ticketFromDatabase = super.findByIdAndAccountId(ticket.getId());
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

    @Override
    public long countEntityWithUser(UUID uuid) {
        var filterByAuthorOfTicket = BaseEntityFilter.builder()
                .typeComparison(EXIST_IN.toString())
                .listOfIdEntities(List.of(uuid))
                .build();
        Specification<Ticket> additionConditionByAuthorOfTicket =
                specFactory.makeSpecification(Ticket.class, AUTHOR, filterByAuthorOfTicket);
        var filterByListOfObserversAndExecutors = ListOfBaseEntityFilter.builder()
                .typeComparison(CONTAINS_ALL_OF_LIST.toString())
                .listOfIdEntities(List.of(uuid))
                .build();
        Specification<Ticket> additionConditionByObserversOfTicket =
                specFactory.makeSpecification(Ticket.class, OBSERVERS, filterByListOfObserversAndExecutors);
        Specification<Ticket> additionConditionByExecutorsOfTicket =
                specFactory.makeSpecification(Ticket.class, EXECUTORS, filterByListOfObserversAndExecutors);

        additionConditionByAuthorOfTicket.or(additionConditionByObserversOfTicket).or(additionConditionByExecutorsOfTicket);

        var pageable = PageRequest.of(1, 25, Sort.by(Sort.Direction.fromString("ASC"), "displayName"));
        var foundTickets = findAllByFilter(additionConditionByAuthorOfTicket, pageable);
        return foundTickets.getTotalElements();
    }
}
