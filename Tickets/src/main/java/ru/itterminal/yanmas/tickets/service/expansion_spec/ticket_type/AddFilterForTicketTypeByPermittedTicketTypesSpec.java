package ru.itterminal.yanmas.tickets.service.expansion_spec.ticket_type;

import lombok.RequiredArgsConstructor;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.expansion_spec.ExpansionSpec;
import ru.itterminal.yanmas.commons.model.filter.StringFilter;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.tickets.model.TicketType;
import ru.itterminal.yanmas.tickets.service.impl.SettingsAccessToTicketTypesServiceImpl;

import static ru.itterminal.yanmas.commons.model.filter.StringFilter.TypeComparisonForStringFilter.TEXT_EQUALS;

@Component
@RequiredArgsConstructor
public class AddFilterForTicketTypeByPermittedTicketTypesSpec implements ExpansionSpec<TicketType> {

    private final SpecificationsFactory specFactory;
    private final SettingsAccessToTicketTypesServiceImpl accessToTicketTypesService;

    @Override
    public Specification<TicketType> expansionSpec(Specification<TicketType> specification, User currentUser) {
        var permittedTicketTypes = accessToTicketTypesService.getPermittedTicketTypes(currentUser.getId());
        if (permittedTicketTypes != null) {
            Specification<TicketType> specificationByListId = null;
            for (TicketType tt : permittedTicketTypes) {
                var filterByListId = StringFilter.builder()
                        .typeComparison(TEXT_EQUALS.toString())
                        .value(tt.getId().toString())
                        .build();
                if (specificationByListId == null) {
                    specificationByListId = specFactory.makeSpecification(TicketType.class, "id", filterByListId);
                } else {
                    var specificationById = specFactory.makeSpecification(TicketType.class, "id", filterByListId);
                    specificationByListId = specificationByListId.or(specificationById);
                }
            }
            specification = specification.and(specificationByListId);
        }
        return specification;
    }
}
