package ru.itterminal.yanmas.tickets.service.expansion_spec.ticket;

import lombok.RequiredArgsConstructor;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.expansion_spec.ExpansionSpec;
import ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.tickets.model.Ticket;

import java.util.List;

import static ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;

@Component
@RequiredArgsConstructor
public class AddFilterByGroupIdIfCurrentUserIsFromOuterGroupAndHasRoleAdminOrExecutorSpec implements ExpansionSpec<Ticket> {

    private final SpecificationsFactory specFactory;

    public static final String GROUP = "group";

    @Override
    public Specification<Ticket> expansionSpec(Specification<Ticket> spec, User currentUser) {
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
        return spec;
    }
}
