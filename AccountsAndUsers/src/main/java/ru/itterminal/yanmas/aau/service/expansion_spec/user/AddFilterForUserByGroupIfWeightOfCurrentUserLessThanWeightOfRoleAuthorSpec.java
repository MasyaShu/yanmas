package ru.itterminal.yanmas.aau.service.expansion_spec.user;

import lombok.RequiredArgsConstructor;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.expansion_spec.ExpansionSpec;
import ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;

import java.util.List;

import static ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;

@Component
@RequiredArgsConstructor
public class AddFilterForUserByGroupIfWeightOfCurrentUserLessThanWeightOfRoleAuthorSpec implements ExpansionSpec<User> {

    private final SpecificationsFactory specFactory;

    @Override
    public Specification<User> expansionSpec(Specification<User> spec, User currentUser) {
        if (currentUser.getRole().getWeight() <= Roles.AUTHOR.getWeight()) {
            var groupFilter = BaseEntityFilter.builder()
                    .typeComparison(EXIST_IN.toString())
                    .listOfIdEntities(List.of(currentUser.getGroup().getId()))
                    .build();
            var groupSpec = specFactory.makeSpecification(User.class, "group", groupFilter);
            spec = spec.and(groupSpec);
        }
        return spec;
    }
}
