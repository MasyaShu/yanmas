package ru.itterminal.yanmas.tickets.service.expansion_spec.ticket;

import lombok.RequiredArgsConstructor;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.expansion_spec.ExpansionSpec;
import ru.itterminal.yanmas.commons.model.filter.ListOfBaseEntityFilter;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.tickets.model.Ticket;

import java.util.List;

import static ru.itterminal.yanmas.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.CONTAINS_ALL_OF_LIST;

@Component
@RequiredArgsConstructor
public class AddFilterForTicketByObserverIdIfCurrentUserHasRoleObserverSpec implements ExpansionSpec<Ticket> {

    private final SpecificationsFactory specFactory;

    public static final String OBSERVERS = "observers";

    @Override
    public Specification<Ticket> expansionSpec(Specification<Ticket> spec, User currentUser) {
        var nameOfRoleOfCurrentUser = currentUser.getRole().getName();
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
