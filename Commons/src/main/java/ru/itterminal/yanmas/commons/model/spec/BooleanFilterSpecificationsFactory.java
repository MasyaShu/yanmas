package ru.itterminal.yanmas.commons.model.spec;

import org.springframework.data.jpa.domain.Specification;

import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.commons.model.filter.BooleanFilter;

public class BooleanFilterSpecificationsFactory {

    public <E extends BaseEntity> Specification<E> makeSpecification (BooleanFilter filter, String field) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get(field), filter.getValue());
    }
}
