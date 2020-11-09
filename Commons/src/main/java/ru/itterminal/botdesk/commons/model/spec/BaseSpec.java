package ru.itterminal.botdesk.commons.model.spec;

import java.util.UUID;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.Predicate;

import org.springframework.data.jpa.domain.Specification;

import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto.FilterByDeleted;

public interface BaseSpec<E extends BaseEntity, A extends BaseEntity> {

    default Specification<E> getEntityByDeletedSpec(FilterByDeleted deleted) {
        return (root, query, criteriaBuilder) -> {
            Predicate pred;
            switch (deleted) {
                case TRUE -> pred = criteriaBuilder.equal(root.get("deleted"), true);
                case FALSE -> pred = criteriaBuilder.equal(root.get("deleted"), false);
                default -> pred = null;
            }
            return pred;
        };
    }

    default Specification<E> getEntityByAccountSpec(UUID accountId) {
        return (root, query, criteriaBuilder) -> {
            Join<E, A> entityJoin = root.join("account");
            return criteriaBuilder.equal(entityJoin.<UUID> get("id"), accountId);
        };
    }

    default Specification<E> getEntityByOutIdSpec(String outId) {
        if (outId == null) {
            return (root, query, criteriaBuilder) -> criteriaBuilder.isNull(root.get("outId"));
        }
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(criteriaBuilder.lower(root.get("outId")),
                "%" + outId.toLowerCase() + "%");
    }
}
