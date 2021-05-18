package ru.itterminal.yanmas.aau.service.business_handler;

import org.springframework.data.jpa.domain.Specification;

import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.commons.model.BaseEntity;

@SuppressWarnings("unused")
public interface EntityBusinessHandler<E extends BaseEntity> {

    default void beforeCreate(E entity, User currentUser) {
    }

    default void afterCreate(E entity, User currentUser) {
    }

    default void beforeUpdate(E entity, User currentUser) {
    }

    default void afterUpdate(E entity, User currentUser) {
    }

    // TODO must be deleted
    default Specification<E> beforeFindAllByFilter(Specification<E> specification, User currentUser) {
        return specification;
    }

}
