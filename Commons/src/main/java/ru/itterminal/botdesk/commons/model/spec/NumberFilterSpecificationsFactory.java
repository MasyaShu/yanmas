package ru.itterminal.botdesk.commons.model.spec;

import org.springframework.data.jpa.domain.Specification;
import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.commons.model.filter.NumberFilter;
import ru.itterminal.botdesk.commons.model.filter.NumberFilter.TypeComparisonForNumberFilter;

import javax.persistence.criteria.Predicate;

import static ru.itterminal.botdesk.commons.model.filter.NumberFilter.TypeComparisonForNumberFilter.fromString;

public class NumberFilterSpecificationsFactory {

    public <E extends BaseEntity> Specification<E> makeSpecification
            (NumberFilter filter, String field) {
        TypeComparisonForNumberFilter typeComparison = fromString(filter.getTypeComparison());
        return switch (typeComparison) {
            case IS_EMPTY -> isEmpty(field);
            case IS_NOT_EMPTY -> isNotEmpty(field);
            case GREATER_THAN -> greatThan(field, filter.getValueOne());
            case GREATER_THAN_OR_EQUAL_TO -> greatThanOrEqual(field, filter.getValueOne());
            case LESS_THAN -> lessThan(field, filter.getValueOne());
            case LESS_THAN_OR_EQUAL_TO -> lessThanOrEqual(field, filter.getValueOne());
            case IS_BETWEEN_INCLUSIVE -> isBetweenInclusive(field, filter.getValueOne(), filter.getValueTwo());
            case IS_BETWEEN_EXCLUSION -> isBetweenExclusion(field, filter.getValueOne(), filter.getValueTwo());
            case IS_NOT_BETWEEN_INCLUSIVE -> isNotBetweenInclusive(field, filter.getValueOne(), filter.getValueTwo());
            case IS_NOT_BETWEEN_EXCLUSION -> isNotBetweenExclusion(field, filter.getValueOne(), filter.getValueTwo());
            case IS_EQUAL_TO -> isEqualTo(field, filter.getValueOne());
            case IS_NOT_EQUAL_TO -> isNotEqualTo(field, filter.getValueOne());
        };
    }

    private <E extends BaseEntity> Specification<E> isEmpty(String field) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.isNull(root.get(field));
    }

    private <E extends BaseEntity> Specification<E> isNotEmpty(String field) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.isNull(root.get(field)).not();
    }

    private <E extends BaseEntity> Specification<E> greatThan(String field, Number valueOne) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.gt(root.get(field), valueOne);
    }

    private <E extends BaseEntity> Specification<E> greatThanOrEqual(String field, Number valueOne) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.ge(root.get(field), valueOne);
    }

    private <E extends BaseEntity> Specification<E> lessThan(String field, Number valueOne) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.lt(root.get(field), valueOne);

    }

    private <E extends BaseEntity> Specification<E> lessThanOrEqual(String field, Number valueOne) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.le(root.get(field), valueOne);
    }

    private <E extends BaseEntity> Specification<E> isBetweenInclusive(String field, Number value, Number valueTwo) {
        return (root, query, criteriaBuilder) -> {
            Predicate predicateGreatThanOrEqual = criteriaBuilder.ge(root.get(field), value);
            Predicate predicateLessThanOrEqual = criteriaBuilder.le(root.get(field), valueTwo);
            return criteriaBuilder.and(predicateGreatThanOrEqual, predicateLessThanOrEqual);
        };
    }

    private <E extends BaseEntity> Specification<E> isBetweenExclusion(String field, Number valueOne, Number valueTwo) {
        return (root, query, criteriaBuilder) -> {
            Predicate predicateGreatThan = criteriaBuilder.gt(root.get(field), valueOne);
            Predicate predicateLessThan = criteriaBuilder.lt(root.get(field), valueTwo);
            return criteriaBuilder.and(predicateGreatThan, predicateLessThan);
        };
    }

    private <E extends BaseEntity> Specification<E> isNotBetweenInclusive(String field, Number valueOne, Number valueTwo) {
        return (root, query, criteriaBuilder) -> {
            Predicate predicateGreatThanOrEqual = criteriaBuilder.le(root.get(field), valueOne);
            Predicate predicateLessThanOrEqual = criteriaBuilder.ge(root.get(field), valueTwo);
            return criteriaBuilder.or(predicateGreatThanOrEqual, predicateLessThanOrEqual);
        };
    }

    private <E extends BaseEntity> Specification<E> isNotBetweenExclusion(String field, Number valueOne, Number valueTwo) {
        return (root, query, criteriaBuilder) -> {
            Predicate predicateGreatThanOrEqual = criteriaBuilder.lt(root.get(field), valueOne);
            Predicate predicateLessThanOrEqual = criteriaBuilder.gt(root.get(field), valueTwo);
            return criteriaBuilder.or(predicateGreatThanOrEqual, predicateLessThanOrEqual);
        };
    }

    private <E extends BaseEntity> Specification<E> isEqualTo(String field, Number valueOne) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get(field), valueOne);
    }

    private <E extends BaseEntity> Specification<E> isNotEqualTo(String field, Number valueOne) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.notEqual(root.get(field), valueOne);
    }

}
