package ru.itterminal.yanmas.tickets.service.impl;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.impl.AccountServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter;
import ru.itterminal.yanmas.commons.model.filter.ListOfBaseEntityFilter;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.files.model.File;
import ru.itterminal.yanmas.files.service.FileServiceImpl;
import ru.itterminal.yanmas.integration.across_modules.RequestsFromModuleAccountAndUsers;
import ru.itterminal.yanmas.tickets.model.Priority;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.repository.TicketRepository;
import ru.itterminal.yanmas.tickets.service.validator.TicketOperationValidator;

import javax.persistence.OptimisticLockException;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;
import static ru.itterminal.yanmas.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.CONTAINS_ALL_OF_LIST;

@Slf4j
@Service
@RequiredArgsConstructor
public class TicketServiceImpl extends CrudServiceWithAccountImpl<Ticket, TicketOperationValidator, TicketRepository>
        implements RequestsFromModuleAccountAndUsers {

    public static final String YOU_MUST_USE_ANOTHER_METHOD_CREATE =
            "You must use method create(Ticket entity, User currentUser)";
    public static final String YOU_MUST_USE_ANOTHER_METHOD_UPDATE =
            "You must use method update(Ticket entity, User currentUser)";
    public static final String BY_USER = " by user: ";
    public static final String AUTHOR = "author";
    public static final String OBSERVERS = "observers";
    public static final String EXECUTORS = "executors";
    public static final String START_REOPEN_TICKET_WITH_ID = "Start reopen ticket with id: %s";
    public static final String FINISH_REOPEN_TICKET_WITH_ID = "Finish reopen ticket with id: %s";

    private final TicketCounterServiceImpl ticketCounterService;
    private final TicketSettingServiceImpl ticketSettingService;
    private final FileServiceImpl fileService;
    private final SpecificationsFactory specFactory;
    private final TicketTypeServiceImpl ticketTypeService;
    private final TicketStatusServiceImpl ticketStatusService;
    private final UserServiceImpl userService;
    private final AccountServiceImpl accountService;

    @Override
    @Deprecated
    public Ticket create(Ticket entity) { //NOSONAR
        throw new UnsupportedOperationException(YOU_MUST_USE_ANOTHER_METHOD_CREATE);
    }

    @Override
    @Deprecated
    public Ticket update(Ticket entity) { //NOSONAR
        throw new UnsupportedOperationException(YOU_MUST_USE_ANOTHER_METHOD_UPDATE);
    }

    @Transactional
    public Ticket create(Ticket entity, User currentUser) {
        log.trace(
                format(
                        CREATE_INIT_MESSAGE,
                        entity.getClass().getSimpleName(),
                        entity.toString() + BY_USER + currentUser.getEmail()
                )
        );
        validator.checkAccessBeforeCreate(entity, currentUser);
        validator.logicalValidationBeforeCreate(entity);
        validator.checkUniqueness(entity);
        var createdEntity = repository.create(entity);
        if (createdEntity.getFiles() != null && !createdEntity.getFiles().isEmpty()) {
            for (File file : createdEntity.getFiles()) {
                file.setEntityId(entity.getId());
                // TODO it must will be
                // fileService.update(file);
            }
        }
        log.trace(format(CREATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), createdEntity.toString()));
        return createdEntity;
    }

    @Transactional
    public Ticket update(Ticket entity, User currentUser) {
        validator.checkAccessBeforeUpdate(entity, currentUser);
        validator.logicalValidationBeforeUpdate(entity);
        log.trace(format(UPDATE_INIT_MESSAGE, entity.getClass().getSimpleName(), entity.getId(), entity));
        try {
            entity.generateDisplayName();
            var updatedEntity = repository.update(entity);
            log.trace(format(UPDATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), entity.getId(), updatedEntity));
            return updatedEntity;
        } catch (OptimisticLockException ex) {
            throw new OptimisticLockingFailureException(format(VERSION_INVALID_MESSAGE, entity.getId()));
        }
    }

    @Transactional
    public Ticket reOpen(Ticket entity, User currentUser) {
        validator.logicalValidationBeforeUpdate(entity);
        log.trace(format(START_REOPEN_TICKET_WITH_ID, entity.getId()));
        var ticketSetting = ticketSettingService.getSettingOrPredefinedValuesForTicket(
                currentUser,
                entity.getAuthor()
        );
        try {
            entity.setIsFinished(false);
            entity.setTicketStatus(ticketSetting.getTicketStatusForReopen());
            var updatedEntity = repository.update(entity);
            log.trace(format(FINISH_REOPEN_TICKET_WITH_ID, entity.getId()));
            return updatedEntity;
        } catch (OptimisticLockException ex) {
            throw new OptimisticLockingFailureException(format(VERSION_INVALID_MESSAGE, entity.getId()));
        }
    }


    @Override
    @Transactional(readOnly = true)
    public long countEntityWithUser(UUID userId) {
        var filterByAuthorOfTicket = BaseEntityFilter.builder()
                .typeComparison(EXIST_IN.toString())
                .listOfIdEntities(List.of(userId))
                .build();
        var specForSearch = specFactory.makeSpecification(Ticket.class, AUTHOR, filterByAuthorOfTicket);
        var filterByListOfObserversAndExecutors = ListOfBaseEntityFilter.builder()
                .typeComparison(CONTAINS_ALL_OF_LIST.toString())
                .listOfIdEntities(List.of(userId))
                .build();
        specForSearch = specForSearch.or(
                specFactory.makeSpecification(Ticket.class, OBSERVERS, filterByListOfObserversAndExecutors)
        );
        specForSearch = specForSearch.or(
                specFactory.makeSpecification(Ticket.class, EXECUTORS, filterByListOfObserversAndExecutors)
        );
        var pageable = PageRequest.of(1, 25, Sort.by(Sort.Direction.fromString("ASC"), "displayName"));
        var foundTickets = findAllByFilter(specForSearch, pageable);
        return foundTickets.getTotalElements();
    }
}
