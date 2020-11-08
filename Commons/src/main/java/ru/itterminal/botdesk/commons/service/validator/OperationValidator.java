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

    boolean beforeCreate(E entity);

    boolean checkUniqueness(E entity);

    boolean beforeUpdate(E entity);

    boolean checkLogicalDelete(UUID id);

}
