package ru.itterminal.botdesk.commons.model.spec;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.springframework.data.jpa.domain.Specification;

import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;

public abstract class GeneralSpec<E extends BaseEntity> {

    public Specification<E> getEntityByDeletedSpec(BaseFilterDto.FilterByDeleted deleted) {
        return new Specification<E>() {
            @Override
            public Predicate toPredicate(Root<E> root,
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
}
