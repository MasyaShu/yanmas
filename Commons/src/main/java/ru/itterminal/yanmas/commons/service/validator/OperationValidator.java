package ru.itterminal.yanmas.commons.service.validator;

import ru.itterminal.yanmas.commons.model.BaseEntity;

public interface OperationValidator<E extends BaseEntity> {

    String INVALID_ENTITY_MESSAGE = "Expected entity, but received null";

    boolean checkUniqueness(E entity);

    boolean logicalValidationBeforeCreate(E entity);

    boolean logicalValidationBeforeUpdate(E entity);

    default void checkAccessBeforeRead(E entity) {}

    default void checkAccessBeforeCreate(E entity) {}

    default void checkAccessBeforeUpdate(E entity) {}

}
