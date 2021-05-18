package ru.itterminal.yanmas.aau.service.expansion_spec.group;

import static ru.itterminal.yanmas.commons.model.filter.StringFilter.TypeComparisonForStringFilter.TEXT_EQUALS;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.expansion_spec.ExpansionSpec;
import ru.itterminal.yanmas.commons.model.filter.StringFilter;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;

@Component
@RequiredArgsConstructor
public class AddFilterByGroupIdIfWeightOfCurrentUserLessThanWeightOfRoleAuthorSpec implements ExpansionSpec<Group> {

    private final SpecificationsFactory specFactory;

    @Override
    public Specification<Group> expansionSpec(Specification<Group> spec, User currentUser) {
        if (currentUser.getRole().getWeight() <= Roles.AUTHOR.getWeight()) {
            var filterById = StringFilter.builder()
                    .typeComparison(TEXT_EQUALS.toString())
                    .value(currentUser.getGroup().getId().toString())
                    .build();
            var specificationById = specFactory.makeSpecification(Group.class, "id", filterById);
            spec = spec.and(specificationById);
        }
        return spec;
    }
}
