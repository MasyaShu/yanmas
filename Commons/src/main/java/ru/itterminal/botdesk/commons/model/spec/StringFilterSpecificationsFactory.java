package ru.itterminal.botdesk.commons.model.spec;

import static ru.itterminal.botdesk.commons.model.filter.StringFilter.TypeComparisonForStringFilter.fromString;

import javax.persistence.criteria.Predicate;

import org.springframework.data.jpa.domain.Specification;

import lombok.val;
import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.commons.model.filter.StringFilter;
import ru.itterminal.botdesk.commons.model.filter.StringFilter.TypeComparisonForStringFilter;

public class StringFilterSpecificationsFactory {

    private static final String EMPTY_STRING = "";

    public <E extends BaseEntity> Specification<E> makeSpecification
            (StringFilter filter, String field, Class<E> entityClass) {
        TypeComparisonForStringFilter typeComparison = fromString(filter.getTypeComparison());
        switch (typeComparison) {
            case IS_NOT_EMPTY:
            case TEXT_CONTAINS:
            case TEXT_NOT_CONTAINS:
            case TEXT_STARTS_WITH:
            case TEXT_ENDS_WITH:
            case TEXT_EQUALS:
            default:
                throw new IllegalStateException("Unexpected value: " + filter.getTypeComparison());
        }
    }

    private <E extends BaseEntity> Specification<E> isEmpty(String field, Class<E> entityClass) {
        return (root, query, criteriaBuilder) -> {
            val objectPathName = root.get(field);
            Predicate predicateForNull = criteriaBuilder.isNull(objectPathName);
            Predicate predicateForEmpty = criteriaBuilder.equal(objectPathName, EMPTY_STRING);
            return criteriaBuilder.or(predicateForEmpty, predicateForNull);
        };
    }

}
