package ru.itterminal.botdesk.commons.service.impl;

import static java.lang.String.format;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.commons.repository.CustomizedParentEntityRepository;
import ru.itterminal.botdesk.commons.service.CrudService;
import ru.itterminal.botdesk.commons.service.validator.OperationValidator;

/**
 * Skeletal implementation of general CRUD operations for each dictionary
 *
 * @param <E> extends BaseEntity
 * @param <V> extends OperationValidate
 * @param <R> extends CustomizedParentEntityRepository
 */
@Slf4j
@Service
@Transactional
public abstract class CrudServiceImpl<E extends BaseEntity,
        V extends OperationValidator<E>,
        R extends CustomizedParentEntityRepository<E>>
        implements CrudService<E> {

    @Autowired
    protected R repository;

    @Autowired
    protected V validator;

    /**
     * Set random UUID for object and save it in database
     * <p> If id or unique fields conflict with  entity that already exist in database will throw exception
     *
     * @param entity extends BaseEntity
     * @return entity from database
     */
    @Override
    public E create(E entity) {
        validator.beforeCreate(entity);
        log.trace(format(CREATE_INIT_MESSAGE, entity.getClass().getSimpleName(), entity.toString()));
        UUID id = UUID.randomUUID();
        entity.setId(id);
        validator.checkUniqueness(entity);
        E createdEntity;
        createdEntity = repository.create(entity);
        log.trace(format(CREATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), createdEntity.toString()));
        return createdEntity;
    }

    /**
     * Get entity from database by it's id
     * <p>Will return entity even if it marked for delete
     * <p>If entity with required Id do not exist will throw exception ResourceNotFoundException
     *
     * @param id of required entity
     * @return entity from database
     */
    @Override
    @Transactional(readOnly = true)
    public E findById(UUID id) {
        String searchParameter = "id";
        log.trace(format(FIND_INIT_MESSAGE, searchParameter, id));
        Optional<E> baseEntity = repository.findById(id);
        if (baseEntity.isPresent()) {
            log.trace(format(FIND_FINISH_MESSAGE, searchParameter, id, baseEntity.get()));
            return baseEntity.get();
        } else {
            String errorMessage = format(FIND_INVALID_MESSAGE, searchParameter, id);
            log.error(errorMessage);
            throw new EntityNotExistException(errorMessage);
        }
    }

    /**
     * Find entities in database by filter
     *
     * @param specification contains conditions for filtering
     * @return page(s) in selected format
     */
    @Override
    @Transactional(readOnly = true)
    public Page<E> findAllByFilter(Specification<E> specification, Pageable pageable) {
        log.trace(format(ALL_FIND_BY_FILTER_INIT_MESSAGE, pageable.getPageSize()));
        Page<E> page = repository.findAll(specification, pageable);
        log.trace(format(ALL_FIND_BY_FILTER_FINISH_MESSAGE, page.getTotalElements(), pageable.getPageNumber()));
        return page;
    }

    /**
     * Get all available entities from database table in selected page format
     * Can't use null as input
     *
     * @param pageable from springframework
     * @return page(s) in selected format
     */
    @Override
    @Transactional(readOnly = true)
    public Page<E> findAll(Pageable pageable) {
        log.trace(format(ALL_FIND_INIT_MESSAGE, pageable.getPageSize()));
        Page<E> page = repository.findAll(pageable);
        log.trace(format(ALL_FIND_FINISH_MESSAGE, page.getTotalElements(), pageable.getPageNumber()));
        return page;
    }

    /**
     * Update existing entity if it haven't been marked as deleted (including in the database)
     *
     * @param entity with new parameters
     * @return updated version of entity
     */
    @Override
    public E update(E entity) {
        validator.beforeUpdate(entity);
        log.trace(format(UPDATE_INIT_MESSAGE, entity.getClass().getSimpleName(), entity.getId(), entity));
        E entityFromDatabase = repository.findById(entity.getId()).orElseThrow(() -> {
            String message = format(ENTITY_NOT_EXIST_MESSAGE, entity.getClass().getSimpleName(), entity.getId());
            log.error(message);
            return new EntityNotExistException(message);
        });
        try {
            E updatedEntity = repository.update(entity);
            log.trace(format(UPDATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), entity.getId(), updatedEntity));
            return updatedEntity;
        }
        catch (ObjectOptimisticLockingFailureException ex) {
            throw new OptimisticLockingFailureException(format(VERSION_INVALID_MESSAGE, entity.getId()));
        }
    }

    public void physicalDelete(UUID id) {
        //TODO physicalDelete will be done in future
        throw new UnsupportedOperationException("physicalDeleteNotYetImplement");
    }

    @Override
    @Transactional(readOnly = true)
    public List<E> findAll() {
        return (List<E>) repository.findAll();
    }
}
