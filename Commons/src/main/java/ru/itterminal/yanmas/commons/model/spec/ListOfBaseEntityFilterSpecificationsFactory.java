package ru.itterminal.yanmas.commons.model.spec;

import java.util.List;
import java.util.UUID;

import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Subquery;

import org.springframework.data.jpa.domain.Specification;

import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.commons.model.filter.ListOfBaseEntityFilter;

@SuppressWarnings({"unused", "DuplicatedCode"})
public class ListOfBaseEntityFilterSpecificationsFactory {

    public static final String ID = "id";

    public <E extends BaseEntity> Specification<E> makeSpecification
            (ListOfBaseEntityFilter filter, String field, Class<E> entityClass) {
        ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter typeComparison = ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.fromString(filter.getTypeComparison());
        return switch (typeComparison) {
            case IS_EMPTY -> isEmpty(entityClass, field);
            case IS_NOT_EMPTY -> isNotEmpty(entityClass, field);
            case IS_EQUAL_TO -> isEqualTo(entityClass, field, filter.getListOfIdEntities());
            case IS_NOT_EQUAL_TO -> isNotEqualTo(entityClass, field, filter.getListOfIdEntities());
            case NOT_CONTAINS_ANY_IN_LIST -> notContainsAnyInList(entityClass, field, filter.getListOfIdEntities());
            case CONTAINS_ANY_IN_LIST -> containsAnyInList(field, filter.getListOfIdEntities());
            case NOT_CONTAINS_ALL_OF_LIST -> notContainsAllOfList(entityClass, field, filter.getListOfIdEntities());
            case CONTAINS_ALL_OF_LIST -> containsAllOfList(entityClass, field, filter.getListOfIdEntities());
        };
    }

    private <E extends BaseEntity> Specification<E> isEmpty
            (Class<E> entityClass, String field) {
        return (root, query, criteriaBuilder) -> {
            Subquery<Long> subQuerySizeOfList = query.subquery(Long.class);
            Root<E> subRootSizeOfList = subQuerySizeOfList.from(entityClass);

            var executors = subRootSizeOfList.joinList(field);

            Predicate ticketsIdIsEqualsPredicate = criteriaBuilder.equal(root.get(ID), subRootSizeOfList.get(ID));

            subQuerySizeOfList.select(criteriaBuilder.count(subRootSizeOfList.get(ID)));
            subQuerySizeOfList.where(ticketsIdIsEqualsPredicate);

            return criteriaBuilder.equal(subQuerySizeOfList, 0);
        };
    }

    private <E extends BaseEntity> Specification<E> isNotEmpty
            (Class<E> entityClass, String field) {
        return (root, query, criteriaBuilder) -> {
            Subquery<Long> subQuerySizeOfList = query.subquery(Long.class);
            Root<E> subRootSizeOfList = subQuerySizeOfList.from(entityClass);

            var executors = subRootSizeOfList.joinList(field);

            Predicate ticketsIdIsEqualsPredicate = criteriaBuilder.equal(root.get(ID), subRootSizeOfList.get(ID));

            subQuerySizeOfList.select(criteriaBuilder.count(subRootSizeOfList.get(ID)));
            subQuerySizeOfList.where(ticketsIdIsEqualsPredicate);

            return criteriaBuilder.notEqual(subQuerySizeOfList, 0);
        };
    }

    private <E extends BaseEntity> Specification<E> isEqualTo
            (Class<E> entityClass, String field, List<UUID> values) {
        return (root, query, criteriaBuilder) -> {
            Long size = (long) values.size();

            Subquery<Long> subQueryAllInList = query.subquery(Long.class);
            Root<E> subRootAllInList = subQueryAllInList.from(entityClass);

            var executors = subRootAllInList.joinList(field);

            Predicate ticketExecutorsInListOfExecutorsPredicate = executors.get(ID).in(values);
            Predicate ticketsIdIsEqualsPredicate = criteriaBuilder.equal(root.get(ID), subRootAllInList.get(ID));

            subQueryAllInList.select(criteriaBuilder.count(subRootAllInList.get(ID)));
            subQueryAllInList.where(ticketExecutorsInListOfExecutorsPredicate, ticketsIdIsEqualsPredicate);

            var allInList = criteriaBuilder.equal(subQueryAllInList, size);

            Subquery<Long> subQuerySizeOfList = query.subquery(Long.class);
            Root<E> subRootSizeOfList = subQuerySizeOfList.from(entityClass);

            var executors2 = subRootSizeOfList.joinList(field);

            Predicate ticketsIdIsEqualsPredicate2 = criteriaBuilder.equal(root.get(ID), subRootSizeOfList.get(ID));

            subQuerySizeOfList.select(criteriaBuilder.count(subRootSizeOfList.get(ID)));
            subQuerySizeOfList.where(ticketsIdIsEqualsPredicate2);

            var sizeOfList = criteriaBuilder.equal(subQuerySizeOfList, size);

            return criteriaBuilder.and(allInList, sizeOfList);
        };
    }

    private <E extends BaseEntity> Specification<E> isNotEqualTo
            (Class<E> entityClass, String field, List<UUID> values) {
        return (root, query, criteriaBuilder) -> {
            Long size = (long) values.size();

            Subquery<Long> subQueryAllInList = query.subquery(Long.class);
            Root<E> subRootAllInList = subQueryAllInList.from(entityClass);

            var executors = subRootAllInList.joinList(field);

            Predicate ticketExecutorsInListOfExecutorsPredicate = executors.get(ID).in(values);
            Predicate ticketsIdIsEqualsPredicate = criteriaBuilder.equal(root.get(ID), subRootAllInList.get(ID));

            subQueryAllInList.select(criteriaBuilder.count(subRootAllInList.get(ID)));
            subQueryAllInList.where(ticketExecutorsInListOfExecutorsPredicate, ticketsIdIsEqualsPredicate);

            var allInList = criteriaBuilder.equal(subQueryAllInList, size);

            Subquery<Long> subQuerySizeOfList = query.subquery(Long.class);
            Root<E> subRootSizeOfList = subQuerySizeOfList.from(entityClass);

            var executors2 = subRootSizeOfList.joinList(field);

            Predicate ticketsIdIsEqualsPredicate2 = criteriaBuilder.equal(root.get(ID), subRootSizeOfList.get(ID));

            subQuerySizeOfList.select(criteriaBuilder.count(subRootSizeOfList.get(ID)));
            subQuerySizeOfList.where(ticketsIdIsEqualsPredicate2);

            var sizeOfList = criteriaBuilder.equal(subQuerySizeOfList, size);

            return criteriaBuilder.and(allInList, sizeOfList).not();
        };
    }

    private <E extends BaseEntity> Specification<E> containsAnyInList
            (String field, List<UUID> values) {
        return (root, query, criteriaBuilder) -> {
            query.distinct(true);
            var objectListJoin = root.joinList(field);
            return objectListJoin.get(ID).in(values);
        };
    }

    private <E extends BaseEntity> Specification<E> notContainsAnyInList
            (Class<E> entityClass, String field, List<UUID> values) {
        return (root, query, criteriaBuilder) -> {
            Subquery<Long> subQuery = query.subquery(Long.class);
            Root<E> subRoot = subQuery.from(entityClass);

            var executors = subRoot.joinList(field);

            Predicate ticketExecutorsInListOfExecutorsPredicate = executors.get(ID).in(values);
            Predicate ticketsIdIsEqualsPredicate = criteriaBuilder.equal(root.get(ID), subRoot.get(ID));

            subQuery.select(criteriaBuilder.count(subRoot.get(ID)));
            subQuery.where(ticketExecutorsInListOfExecutorsPredicate, ticketsIdIsEqualsPredicate);

            return criteriaBuilder.equal(subQuery, 0);
        };
    }

    private <E extends BaseEntity> Specification<E> containsAllOfList
            (Class<E> entityClass, String field, List<UUID> values) {
        return (root, query, criteriaBuilder) -> {
            Subquery<Long> subQuery = query.subquery(Long.class);
            Root<E> subRoot = subQuery.from(entityClass);

            var objectListJoin = subRoot.joinList(field);

            var ticketExecutorsInListOfExecutorsPredicate = objectListJoin.get(ID).in(values);
            var ticketsIdIsEqualsPredicate = criteriaBuilder.equal(root.get(ID), subRoot.get(ID));

            subQuery.select(criteriaBuilder.count(subRoot.get(ID)));
            subQuery.where(ticketExecutorsInListOfExecutorsPredicate, ticketsIdIsEqualsPredicate);

            Long size = (long) values.size();

            return criteriaBuilder.equal(subQuery, size);
        };
    }

    private <E extends BaseEntity> Specification<E> notContainsAllOfList
            (Class<E> entityClass, String field, List<UUID> values) {
        return (root, query, criteriaBuilder) -> {
            Subquery<Long> subQuery = query.subquery(Long.class);
            Root<E> subRoot = subQuery.from(entityClass);

            var executors = subRoot.joinList(field);

            Predicate ticketExecutorsInListOfExecutorsPredicate = executors.get(ID).in(values);
            Predicate ticketsIdIsEqualsPredicate = criteriaBuilder.equal(root.get(ID), subRoot.get(ID));

            subQuery.select(criteriaBuilder.count(subRoot.get(ID)));
            subQuery.where(ticketExecutorsInListOfExecutorsPredicate, ticketsIdIsEqualsPredicate);

            Long size = (long) values.size();

            return criteriaBuilder.lessThan(subQuery, size);
        };
    }

}
