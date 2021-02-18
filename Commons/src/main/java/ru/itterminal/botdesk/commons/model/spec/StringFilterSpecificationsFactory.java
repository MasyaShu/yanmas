package ru.itterminal.botdesk.commons.model.spec;

import static ru.itterminal.botdesk.commons.model.filter.StringFilter.TypeComparisonForStringFilter.fromString;

import javax.persistence.criteria.Predicate;

import org.springframework.data.jpa.domain.Specification;

import lombok.val;
import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.commons.model.filter.StringFilter;

import java.util.UUID;

public class StringFilterSpecificationsFactory {

    private static final String EMPTY_STRING = "";

    public <E extends BaseEntity> Specification<E> makeSpecification
            (StringFilter filter, String field) {
        var typeComparison = fromString(filter.getTypeComparison());
        return switch (typeComparison) {
            case IS_EMPTY -> isEmpty(field);
            case IS_NOT_EMPTY -> isNotEmpty(field);
            case TEXT_CONTAINS -> textContains(field, filter.getValue());
            case TEXT_NOT_CONTAINS -> textNotContains(field, filter.getValue());
            case TEXT_STARTS_WITH -> textStartsWith(field, filter.getValue());
            case TEXT_ENDS_WITH -> textEndsWith(field, filter.getValue());
            case TEXT_EQUALS -> textEquals(field, filter.getValue());
        };
    }

    private <E extends BaseEntity> Specification<E> isEmpty(String field) {
        return (root, query, criteriaBuilder) -> {
            val objectPathName = root.get(field);
            Predicate predicateForNull = criteriaBuilder.isNull(objectPathName);
            Predicate predicateForEmpty = criteriaBuilder.equal(objectPathName, EMPTY_STRING);
            return criteriaBuilder.or(predicateForEmpty, predicateForNull);
        };
    }

    private <E extends BaseEntity> Specification<E> isNotEmpty(String field) {
        return (root, query, criteriaBuilder) -> {
            val objectPathName = root.get(field);
            Predicate predicateForNull = criteriaBuilder.isNull(objectPathName);
            Predicate predicateForEmpty = criteriaBuilder.equal(objectPathName, EMPTY_STRING);
            return criteriaBuilder.or(predicateForEmpty, predicateForNull).not();
        };
    }

    private <E extends BaseEntity> Specification<E> textContains(String field, String value) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.like(
                        criteriaBuilder.lower(root.get(field)),
                        "%" + value.toLowerCase() + "%"
                );
    }

    private <E extends BaseEntity> Specification<E> textNotContains(String field, String value) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.like(
                        criteriaBuilder.lower(root.get(field)),
                        "%" + value.toLowerCase() + "%"
                ).not();
    }

    private <E extends BaseEntity> Specification<E> textStartsWith(String field, String value) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.like(
                        criteriaBuilder.lower(root.get(field)),
                        value.toLowerCase() + "%"
                );
    }

    private <E extends BaseEntity> Specification<E> textEndsWith(String field, String value) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.like(
                        criteriaBuilder.lower(root.get(field)),
                        "%" + value.toLowerCase()
                );
    }

    private <E extends BaseEntity> Specification<E> textEquals(String field, String value) {
        if (field.equals("id")) {
            var valueUUID = UUID.fromString(value);
            return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get(field), valueUUID);
        }
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get(field), value);
    }
}
