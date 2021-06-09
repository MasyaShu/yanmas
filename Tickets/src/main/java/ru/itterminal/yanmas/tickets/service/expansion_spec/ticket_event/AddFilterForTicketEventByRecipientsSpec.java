package ru.itterminal.yanmas.tickets.service.expansion_spec.ticket_event;

import lombok.RequiredArgsConstructor;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.expansion_spec.ExpansionSpec;
import ru.itterminal.yanmas.commons.model.filter.ListOfBaseEntityFilter;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.tickets.model.TicketEvent;

import java.util.List;

import static ru.itterminal.yanmas.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.CONTAINS_ALL_OF_LIST;
import static ru.itterminal.yanmas.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.IS_EMPTY;

@Component
@RequiredArgsConstructor
public class AddFilterForTicketEventByRecipientsSpec implements ExpansionSpec<TicketEvent> {

    public static final String RECIPIENTS = "recipients";
    private final SpecificationsFactory specFactory;

    @Override
    public Specification<TicketEvent> expansionSpec(Specification<TicketEvent> spec, User currentUser) {

        var filterByEmptyList = ListOfBaseEntityFilter.builder()
                .typeComparison(IS_EMPTY.toString())
                .build();
        Specification<TicketEvent> additionConditionByEmptyListOfTicketEvent =
                specFactory.makeSpecification(TicketEvent.class, RECIPIENTS, filterByEmptyList);
        var filterByListOfRecipients = ListOfBaseEntityFilter.builder()
                .typeComparison(CONTAINS_ALL_OF_LIST.toString())
                .listOfIdEntities(List.of(currentUser.getId()))
                .build();
        Specification<TicketEvent> additionConditionByRecipientsOfTicketEvent =
                specFactory.makeSpecification(TicketEvent.class, RECIPIENTS, filterByListOfRecipients);
        spec = spec.and(additionConditionByEmptyListOfTicketEvent.or(additionConditionByRecipientsOfTicketEvent));

        return  spec;
    }
}
