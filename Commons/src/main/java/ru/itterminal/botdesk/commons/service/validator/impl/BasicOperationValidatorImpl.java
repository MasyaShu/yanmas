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

    public static final String DEFAULT_CREATE_MESSAGE = "Using default validation for crate method";
    public static final String DEFAULT_UPDATE_MESSAGE = "Using default validation for update method";
    public static final String DEFAULT_LOGICAL_DELETE_MESSAGE = "Using default validation for logical delete method";
    public static final String CHECK_UNIQUENESS = "checkUniqueness({})";
    public static final String FIELDS_UNIQUE = "Fields are unique: {}";
    public static final String FIELDS_NOT_UNIQUE = "Fields not unique: {}";
    public static final String NOT_UNIQUE_CODE = "not unique";
    public static final String LOGIC_CONSTRAINT_CODE = "logic constraint";
    public static final String NOT_UNIQUE_MESSAGE = "%s is occupied";
    public static final String VALIDATION_FAILED = "Validation failed";
    public static final String FIELDS_ARE_NOT_VALID = "Fields are not valid: {}";


    @Override
    public boolean beforeCreate(E entity) {
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
    public boolean beforeUpdate(E entity) {
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

}
