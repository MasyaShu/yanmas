package ru.itterminal.yanmas.commons.model.spec;

import org.springframework.data.jpa.domain.Specification;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter;

import java.util.List;
import java.util.UUID;

@SuppressWarnings({"unused", "DuplicatedCode"})
public class BaseEntityFilterSpecificationsFactory {

    public static final String ID = "id";

    public <E extends BaseEntity> Specification<E> makeSpecification
            (BaseEntityFilter filter, String field) {
        BaseEntityFilter.TypeComparisonForBaseEntityFilter typeComparison = BaseEntityFilter.TypeComparisonForBaseEntityFilter.fromString(filter.getTypeComparison());
        return switch (typeComparison) {
            case IS_EMPTY -> isEmpty(field);
            case IS_NOT_EMPTY -> isNotEmpty(field);
            case EXIST_IN -> existIn(field, filter.getListOfIdEntities());
            case NOT_EXIST_IN -> notExistIn(field, filter.getListOfIdEntities());
        };
    }

    private <E extends BaseEntity> Specification<E> isEmpty
            (String field) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.isNull(root.get(field));
    }

    private <E extends BaseEntity> Specification<E> isNotEmpty
            (String field) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.isNull(root.get(field)).not();
    }

    private <E extends BaseEntity> Specification<E> existIn
            (String field, List<UUID> values) {
        return (root, query, criteriaBuilder) -> root.get(field).<UUID>get("id").in(values);
    }
    private <E extends BaseEntity> Specification<E> notExistIn
            (String field, List<UUID> values) {
        return (root, query, criteriaBuilder) -> root.get(field).<UUID>get("id").in(values).not();
    }



}
