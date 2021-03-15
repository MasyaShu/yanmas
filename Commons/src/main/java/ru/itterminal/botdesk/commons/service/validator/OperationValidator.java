package ru.itterminal.botdesk.commons.service.validator;

import ru.itterminal.botdesk.commons.model.BaseEntity;

@SuppressWarnings("unused")
public interface OperationValidator<E extends BaseEntity> {

    String INVALID_ENTITY_MESSAGE = "Expected entity, but received null";

    boolean beforeCreate(E entity);

    boolean checkUniqueness(E entity);

    boolean beforeUpdate(E entity);

    default void checkAccessForRead(E entity) {}

    default void checkAccessForCreate(E entity) {}

    default void checkAccessForUpdate(E entity) {}


}
