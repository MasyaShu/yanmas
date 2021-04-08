package ru.itterminal.yanmas.tickets.service.impl;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;
import static ru.itterminal.yanmas.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.CONTAINS_ALL_OF_LIST;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.impl.AccountServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.WhoWatchedEntityServiceImpl;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter;
import ru.itterminal.yanmas.commons.model.filter.ListOfBaseEntityFilter;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.files.model.File;
import ru.itterminal.yanmas.files.service.FileServiceImpl;
import ru.itterminal.yanmas.integration.across_modules.RequestsFromModuleAccountAndUsers;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.repository.TicketRepository;
import ru.itterminal.yanmas.tickets.service.validator.TicketOperationValidator;

@SuppressWarnings("unused")
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

    private final TicketCounterServiceImpl ticketCounterService;
    private final TicketSettingServiceImpl ticketSettingService;
    private final WhoWatchedEntityServiceImpl whoWatchedEntityService;
    private final FileServiceImpl fileService;
    private final SpecificationsFactory specFactory;
    private final TicketTypeServiceImpl ticketTypeService;
    private final TicketStatusServiceImpl ticketStatusService;
    private final UserServiceImpl userService;
    private final AccountServiceImpl accountService;
    private final JwtUserBuilder jwtUserBuilder;

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
        log.trace(
                format(
                        CREATE_INIT_MESSAGE,
                        entity.getClass().getSimpleName(),
                        entity.toString() + BY_USER + currentUser.getEmail()
                )
        );
        setNestedObjectsOfEntityBeforeCreate(entity);
        validator.checkAccessBeforeCreate(entity);
        validator.logicalValidationBeforeCreate(entity);
        validator.checkUniqueness(entity);
        var createdEntity = repository.create(entity);
        if (createdEntity.getFiles() != null && !createdEntity.getFiles().isEmpty()) {
            for (File file : createdEntity.getFiles()) {
                file.setEntityId(entity.getId());
                fileService.update(file);
            }
        }
        whoWatchedEntityService.watched(List.of(createdEntity.getId()));
        log.trace(format(CREATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), createdEntity.toString()));
        return createdEntity;
    }

    @Transactional
    public Ticket update(Ticket entity, User currentUser) {
        // whoWatchedEntityService.watched(List.of(updatedEntity.getId()));
        return null;
    }

    @Override
    public Ticket findByIdAndAccountId(UUID id) {
        var foundTicket =  super.findByIdAndAccountId(id);
        whoWatchedEntityService.watched(List.of(foundTicket.getId()));
        return foundTicket;
    }

    @Override
    protected void setNestedObjectsOfEntityBeforeCreate(Ticket ticket) {
        var currentUser = userService.findByIdAndAccountId(jwtUserBuilder.getJwtUser().getId());
        ticket.setAccount(accountService.findById(jwtUserBuilder.getJwtUser().getAccountId()));
        ticket.setAuthor(userService.findByIdAndAccountId(ticket.getAuthor().getId()));
        ticket.setGroup(ticket.getAuthor().getGroup());
        ticket.setId(UUID.randomUUID());
        ticket.generateDisplayName();
        ticket.setNumber(ticketCounterService.getNextTicketNumber(ticket.getAccount().getId()));
        var ticketSetting = ticketSettingService.getSettingOrPredefinedValuesForTicket(
                ticket.getAccount().getId(),
                ticket.getGroup().getId(),
                ticket.getAuthor().getId()
        );
        // ticket.status
        boolean isCurrentUserFromInnerGroup = currentUser.getGroup().getIsInner();
        var isTicketFinished = ticket.getIsFinished();
        var ticketStatus = ticket.getTicketStatus();
        var weightOfRoleOfCurrentUser = currentUser.getRole().getWeight();
        if (Boolean.TRUE.equals(isTicketFinished) && (weightOfRoleOfCurrentUser >= Roles.EXECUTOR.getWeight())) {
            ticket.setTicketStatus(ticketSetting.getTicketStatusForClose());
        } else if (ticketStatus != null && Boolean.FALSE.equals(isTicketFinished) && isCurrentUserFromInnerGroup
                && (weightOfRoleOfCurrentUser >= Roles.EXECUTOR.getWeight())) {
            ticket.setTicketStatus(ticketStatusService.findByIdAndAccountId(ticketStatus.getId()));
        } else if ((Boolean.FALSE.equals(isTicketFinished) && ticket.getTicketStatus() == null)
                || (Boolean.FALSE.equals(isTicketFinished) && !isCurrentUserFromInnerGroup)
                || (Boolean.FALSE.equals(isTicketFinished) && weightOfRoleOfCurrentUser == Roles.AUTHOR.getWeight())) {
            ticket.setTicketStatus(ticketSetting.getTicketStatusForNew());
        }
        // ticket.ticketType
        var ticketType = ticket.getTicketType();
        if (ticketType == null || !isCurrentUserFromInnerGroup
                || weightOfRoleOfCurrentUser == Roles.AUTHOR.getWeight()) {
            ticket.setTicketType(ticketSetting.getTicketTypeForNew());
        } else if (weightOfRoleOfCurrentUser >= Roles.EXECUTOR.getWeight()) {
            ticket.setTicketType(ticketTypeService.findByIdAndAccountId(ticketType.getId()));
        }
        // ticket.observers
        if (ticket.getObservers() == null || !isCurrentUserFromInnerGroup
                || weightOfRoleOfCurrentUser == Roles.AUTHOR.getWeight()) {
            ticket.setObservers(ticketSetting.getObservers());
        } else if (weightOfRoleOfCurrentUser >= Roles.EXECUTOR.getWeight()) {
            var listObserversId = ticket.getObservers().stream()
                    .map(BaseEntity::getId)
                    .collect(Collectors.toList());
            ticket.setObservers(userService.findAllByAccountIdAndListId(listObserversId));
        }
        // ticket.executors
        if (ticket.getExecutors() == null || !isCurrentUserFromInnerGroup
                || weightOfRoleOfCurrentUser == Roles.AUTHOR.getWeight()) {
            ticket.setExecutors(ticketSetting.getExecutors());
        } else if (weightOfRoleOfCurrentUser >= Roles.EXECUTOR.getWeight()) {
            var listExecutorsId = ticket.getExecutors().stream()
                    .map(BaseEntity::getId)
                    .collect(Collectors.toList());
            ticket.setExecutors(userService.findAllByAccountIdAndListId(listExecutorsId));
        }
    }

    @Override
    public long countEntityWithUser(UUID uuid) {
        var filterByAuthorOfTicket = BaseEntityFilter.builder()
                .typeComparison(EXIST_IN.toString())
                .listOfIdEntities(List.of(uuid))
                .build();
        var specForSearch = specFactory.makeSpecification(Ticket.class, AUTHOR, filterByAuthorOfTicket);
        var filterByListOfObserversAndExecutors = ListOfBaseEntityFilter.builder()
                .typeComparison(CONTAINS_ALL_OF_LIST.toString())
                .listOfIdEntities(List.of(uuid))
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
