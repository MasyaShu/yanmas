package ru.itterminal.yanmas.tickets.service.business_handler;

import lombok.RequiredArgsConstructor;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.commons.model.filter.StringFilter;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.tickets.model.TicketType;
import ru.itterminal.yanmas.tickets.service.impl.SettingsAccessToTicketTypesServiceImpl;

import static ru.itterminal.yanmas.commons.model.filter.StringFilter.TypeComparisonForStringFilter.TEXT_EQUALS;

@Component
@RequiredArgsConstructor
public class TicketTypeBusinessHandler implements EntityBusinessHandler<TicketType> {

    private final SettingsAccessToTicketTypesServiceImpl accessToTicketTypesService;
    private final SpecificationsFactory specFactory;

    @Override
    public Specification<TicketType> beforeFindAllByFilter(Specification<TicketType> specification, User currentUser) {

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
