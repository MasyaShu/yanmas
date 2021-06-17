package ru.itterminal.yanmas.tickets.service.impl;

import lombok.RequiredArgsConstructor;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.CrudServiceWithBusinessHandlerImpl;
import ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter;
import ru.itterminal.yanmas.commons.model.filter.ListOfBaseEntityFilter;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.integration.across_modules.RequestsFromModuleAccountAndUsers;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.repository.TicketRepository;

import javax.persistence.OptimisticLockException;
import java.util.List;
import java.util.UUID;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;
import static ru.itterminal.yanmas.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.CONTAINS_ALL_OF_LIST;

@Service
@RequiredArgsConstructor
public class TicketServiceImpl extends CrudServiceWithBusinessHandlerImpl<Ticket, TicketRepository>
        implements RequestsFromModuleAccountAndUsers {

    public static final String AUTHOR = "author";
    public static final String OBSERVERS = "observers";
    public static final String EXECUTORS = "executors";

    private final TicketSettingServiceImpl ticketSettingService;
    private final SpecificationsFactory specFactory;

    @SuppressWarnings("unused")
    @Transactional
    public Ticket reOpen(Ticket entity, User currentUser) {
        var ticketSetting = ticketSettingService.getSettingOrPredefinedValuesForTicket(
                currentUser,
                entity.getAuthor()
        );
        try {
            entity.setIsFinished(false);
            entity.setTicketStatus(ticketSetting.getTicketStatusForReopen());
            return repository.update(entity);
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
        var foundTickets = repository.findAll(specForSearch, pageable);
        return foundTickets.getTotalElements();
    }
}
