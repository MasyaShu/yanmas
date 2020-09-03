package ru.itterminal.botdesk.commons.service.validator;

import java.util.UUID;

import ru.itterminal.botdesk.commons.model.BaseEntity;

/**
 * Template for validation on service layer
 *
 * @param <E> extends BaseEntity
 */
public interface OperationValidator<E extends BaseEntity> {

    String INVALID_ENTITY_MESSAGE = "Expected entity, but received null";

    boolean checkBeforeCreate(E entity);

    boolean checkUniqueness(E entity);

    boolean checkBeforeUpdate(E entity);

    boolean checkLogicalDelete(UUID id);

    boolean checkPhysicalDelete(UUID id);
}
