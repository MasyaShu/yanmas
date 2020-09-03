package ru.itterminal.botdesk.commons.service.validator.impl;

import java.util.UUID;

import org.springframework.stereotype.Component;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.exception.NullEntityException;
import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.commons.service.validator.OperationValidator;

/**
 * Basic implementation of service layer validator
 * <p> check only if variable not null
 *
 * @param <E> extends BaseEntity
 */
@Slf4j
@Component
public class BasicOperationValidatorImpl<E extends BaseEntity> implements OperationValidator<E> {

    private static final String DEFAULT_CREATE_MESSAGE = "Using default validation for crate method";
    private static final String DEFAULT_UPDATE_MESSAGE = "Using default validation for update method";
    private static final String DEFAULT_LOGICAL_DELETE_MESSAGE = "Using default validation for logical delete method";
    private static final String PHYSICAL_DELETE_MESSAGE = "Using default validation for physical delete method";
    protected static final String CHECK_UNIQUENESS = "checkUniqueness({})";
    protected static final String FIELDS_UNIQUE = "Fields are unique: {}";
    protected static final String FIELDS_NOT_UNIQUE = "Fields not unique: {}";
    protected static final String NOT_UNIQUE_CODE = "not unique";
    protected static final String NOT_UNIQUE_MESSAGE = "%s is occupied";
    protected static final String VALIDATION_FAILED = "Validation failed";


    @Override
    public boolean checkBeforeCreate(E entity) {
        log.debug(DEFAULT_CREATE_MESSAGE);
        if (entity == null) {
            log.error(INVALID_ENTITY_MESSAGE);
            throw new NullEntityException(INVALID_ENTITY_MESSAGE);
        }
        return true;
    }

    @Override
    public boolean checkUniqueness(E entity) {
        return true;
    }

    @Override
    public boolean checkBeforeUpdate(E entity) {
        log.debug(DEFAULT_UPDATE_MESSAGE);
        if (entity == null) {
            log.error(INVALID_ENTITY_MESSAGE);
            throw new NullEntityException(INVALID_ENTITY_MESSAGE);
        }
        return checkUniqueness(entity);
    }

    @Override
    public boolean checkLogicalDelete(UUID id) {
        log.debug(DEFAULT_LOGICAL_DELETE_MESSAGE);
        if (id == null) {
            log.error(INVALID_ENTITY_MESSAGE);
            throw new NullEntityException(INVALID_ENTITY_MESSAGE);
        }
        return true;
    }

    @Override
    public boolean checkPhysicalDelete(UUID id) {
        log.debug(PHYSICAL_DELETE_MESSAGE);
        if (id == null) {
            log.error(INVALID_ENTITY_MESSAGE);
            throw new NullEntityException(INVALID_ENTITY_MESSAGE);
        }
        return true;
    }
}
