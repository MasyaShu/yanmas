package ru.itterminal.botdesk.commons.model.spec;

import org.springframework.data.jpa.domain.Specification;

import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.commons.model.filter.BooleanFilter;

public class BooleanFilterSpecificationsFactory {

    public <E extends BaseEntity> Specification<E> makeSpecification (BooleanFilter filter, String field) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get(field), filter.getValue());
    }
}
