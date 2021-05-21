package ru.itterminal.yanmas.aau.service.business_handler;

import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.commons.model.BaseEntity;

public interface EntityBusinessHandler<E extends BaseEntity> {

    default E beforeCreate(E entity, User currentUser) {
        return entity;
    }

    default void afterCreate(E entity, User currentUser) {
    }

    default E beforeUpdate(E entity, User currentUser) {
        return entity;
    }

    @SuppressWarnings("unused")
    default void afterUpdate(E entity, User currentUser) {
    }
}
