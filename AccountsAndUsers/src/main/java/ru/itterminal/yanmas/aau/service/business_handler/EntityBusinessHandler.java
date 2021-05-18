package ru.itterminal.yanmas.aau.service.business_handler;

import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.commons.model.BaseEntity;

public interface EntityBusinessHandler<E extends BaseEntity> {

    default void beforeCreate(E entity, User currentUser) {
    }

    default void afterCreate(E entity, User currentUser) {
    }

    default void beforeUpdate(E entity, User currentUser) {
    }

    @SuppressWarnings("unused")
    default void afterUpdate(E entity, User currentUser) {
    }
}
