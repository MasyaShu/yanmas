package ru.itterminal.botdesk.commons.model.spec;

import java.util.UUID;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.Predicate;

import org.springframework.data.jpa.domain.Specification;

import lombok.val;
import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;

public interface BaseSpec<E extends BaseEntity, A extends BaseEntity> {

    String OUT_ID = "outId";
    String DELETED = "deleted";
    String ACCOUNT = "account";
    String ID = "id";
    String EMPTY_STRING = "";

    default Specification<E> getEntityByDeletedSpec(BaseFilterDto.FilterByDeleted deleted) {
        return (root, query, criteriaBuilder) -> {
            Predicate pred;
            val objectPathDeleted = root.get(DELETED);
            switch (deleted) {
                case TRUE -> pred = criteriaBuilder.equal(objectPathDeleted, true);
                case FALSE -> pred = criteriaBuilder.equal(objectPathDeleted, false);
                default -> pred = null;
            }
            return pred;
        };
    }

    default Specification<E> getEntityByAccountSpec(UUID accountId) {
        return (root, query, criteriaBuilder) -> {
            Join<E, A> entityJoin = root.join(ACCOUNT);
            return criteriaBuilder.equal(entityJoin.<UUID> get(ID), accountId);
        };
    }

    @SuppressWarnings("DuplicatedCode")
    default Specification<E> getEntityByOutIdSpec(String outId) {
        return (root, query, criteriaBuilder) -> {
            val objectPathOutId = root.get(OUT_ID);
            if (outId.isEmpty()) {
                Predicate predicateForNull =  criteriaBuilder.isNull(objectPathOutId);
                Predicate predicateForEmpty =  criteriaBuilder.equal(objectPathOutId, EMPTY_STRING);
                return criteriaBuilder.or(predicateForEmpty, predicateForNull);
            }
            return criteriaBuilder.like(criteriaBuilder.lower(root.get(OUT_ID)),
                    "%" + outId.toLowerCase() + "%");
        };
    }
}
