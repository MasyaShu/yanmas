package ru.itterminal.yanmas.tickets.service.business_handler;

import static ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;
import static ru.itterminal.yanmas.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.CONTAINS_ALL_OF_LIST;

import java.util.List;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter;
import ru.itterminal.yanmas.commons.model.filter.ListOfBaseEntityFilter;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.files.model.File;
import ru.itterminal.yanmas.files.service.FileServiceImpl;
import ru.itterminal.yanmas.tickets.model.Ticket;

@Component
@RequiredArgsConstructor
public class TicketBusinessHandler implements EntityBusinessHandler<Ticket> {

    private final FileServiceImpl fileService;
    private final SpecificationsFactory specFactory;

    public static final String GROUP = "group";
    public static final String AUTHOR = "author";
    public static final String OBSERVERS = "observers";

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
    public Specification<Ticket> beforeFindAllByFilter(Specification<Ticket> spec, User currentUser) {
        var isCurrentUserFromInnerGroup = currentUser.getGroup().getIsInner();
        var nameOfRoleOfCurrentUser = currentUser.getRole().getName();

        if ((nameOfRoleOfCurrentUser.equals(Roles.ADMIN.toString())
                || nameOfRoleOfCurrentUser.equals(Roles.EXECUTOR.toString())
        ) && Boolean.FALSE.equals(isCurrentUserFromInnerGroup)) {
            var filterByGroupOfCurrentUser = BaseEntityFilter.builder()
                    .typeComparison(EXIST_IN.toString())
                    .listOfIdEntities(List.of(currentUser.getGroup().getId()))
                    .build();
            Specification<Ticket> additionConditionByGroupOfCurrentUser =
                    specFactory.makeSpecification(Ticket.class, GROUP, filterByGroupOfCurrentUser);
            spec = spec.and(additionConditionByGroupOfCurrentUser);
        }

        if (nameOfRoleOfCurrentUser.equals(Roles.AUTHOR.toString())) {
            var filterByAuthorOfTicket = BaseEntityFilter.builder()
                    .typeComparison(EXIST_IN.toString())
                    .listOfIdEntities(List.of(currentUser.getId()))
                    .build();
            Specification<Ticket> additionConditionByAuthorOfTicket =
                    specFactory.makeSpecification(Ticket.class, AUTHOR, filterByAuthorOfTicket);
            var filterByListOfObservers = ListOfBaseEntityFilter.builder()
                    .typeComparison(CONTAINS_ALL_OF_LIST.toString())
                    .listOfIdEntities(List.of(currentUser.getId()))
                    .build();
            Specification<Ticket> additionConditionByObserversOfTicket =
                    specFactory.makeSpecification(Ticket.class, OBSERVERS, filterByListOfObservers);
            spec = spec.and(additionConditionByAuthorOfTicket.or(additionConditionByObserversOfTicket));
        }

        if (nameOfRoleOfCurrentUser.equals(Roles.OBSERVER.toString())) {
            var filterByListOfObservers = ListOfBaseEntityFilter.builder()
                    .typeComparison(CONTAINS_ALL_OF_LIST.toString())
                    .listOfIdEntities(List.of(currentUser.getId()))
                    .build();
            Specification<Ticket> additionConditionByObserversOfTicket =
                    specFactory.makeSpecification(Ticket.class, OBSERVERS, filterByListOfObservers);
            spec = spec.and(additionConditionByObserversOfTicket); //NOSONAR
        }
        return spec;
    }
}
