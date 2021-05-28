package ru.itterminal.yanmas.tickets.service.expansion_spec.ticket;

import lombok.RequiredArgsConstructor;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.expansion_spec.ExpansionSpec;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.service.impl.SettingsAccessToTicketTypesServiceImpl;

import java.util.stream.Collectors;

import static ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;

@Component
@RequiredArgsConstructor
public class AddFilterForTicketByPermittedTicketTypesSpec implements ExpansionSpec<Ticket> {

    private final SpecificationsFactory specFactory;
    private final SettingsAccessToTicketTypesServiceImpl settingsAccessToTicketTypesService;

    public static final String TICKET_TYPE = "ticketType";

    @Override
    public Specification<Ticket> expansionSpec(Specification<Ticket> spec, User currentUser) {
        var permittedTicketTypes = settingsAccessToTicketTypesService.getPermittedTicketTypes(currentUser.getId());
        if (permittedTicketTypes!=null && !permittedTicketTypes.isEmpty()) {
            var permittedTicketTypesId = permittedTicketTypes.stream()
                    .map(BaseEntity::getId)
                    .collect(Collectors.toList());
            var filterByListOfTicketTypes = BaseEntityFilter.builder()
                    .typeComparison(EXIST_IN.toString())
                    .listOfIdEntities(permittedTicketTypesId)
                    .build();
            Specification<Ticket> additionConditionByListOfTicketTypes =
                    specFactory.makeSpecification(Ticket.class, TICKET_TYPE, filterByListOfTicketTypes);
            spec = spec.and(additionConditionByListOfTicketTypes); //NOSONAR
        }
        return spec;
    }
}
