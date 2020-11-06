package ru.itterminal.botdesk.commons.model.spec;

import java.util.UUID;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.springframework.data.jpa.domain.Specification;

import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;

// TODO rename to BaseSpec
public abstract class GeneralSpec<Entity extends BaseEntity, Account extends BaseEntity> {

    public Specification<Entity> getEntityByDeletedSpec(BaseFilterDto.FilterByDeleted deleted) {
        return new Specification<Entity>() {
            @Override
            public Predicate toPredicate(Root<Entity> root,
                CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) {
                Predicate predicate = null;
                if (deleted.equals(BaseFilterDto.FilterByDeleted.ALL)) {
                    return predicate;
                }
                if (deleted.equals(BaseFilterDto.FilterByDeleted.TRUE)) {
                    predicate = criteriaBuilder.equal(root.get("deleted"), true);
                    return predicate;
                }
                predicate = criteriaBuilder.equal(root.get("deleted"), false);
                return predicate;
            }
        };
    }

    public Specification<Entity> getEntityByAccountSpec(UUID accountId) {
        return new Specification<Entity>() {
            @Override
            public Predicate toPredicate(Root<Entity> root,
                    CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) {
                Join<Entity, Account> entityJoin = root.join("account");
                Predicate predicate = criteriaBuilder.equal(entityJoin.<UUID> get("id"), accountId);
                return predicate;
            }
        };
    }

    // TODO add spec for outId
}
