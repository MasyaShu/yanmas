package ru.itterminal.botdesk.commons.service.impl;

import static java.lang.String.format;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import javax.persistence.OptimisticLockException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.commons.repository.CustomizedParentEntityRepository;
import ru.itterminal.botdesk.commons.service.CrudService;
import ru.itterminal.botdesk.commons.service.validator.OperationValidator;

@SuppressWarnings("SpringJavaAutowiredFieldsWarningInspection")
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

    @Override
    public E create(E entity) {
        validator.beforeCreate(entity);
        log.trace(format(CREATE_INIT_MESSAGE, entity.getClass().getSimpleName(), entity.toString()));
        UUID id = UUID.randomUUID();
        entity.setId(id);
        entity.generateDisplayName();
        validator.checkUniqueness(entity);
        E createdEntity;
        createdEntity = repository.create(entity);
        log.trace(format(CREATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), createdEntity.toString()));
        return createdEntity;
    }

    @Override
    public E update(E entity) {
        validator.beforeUpdate(entity);
        log.trace(format(UPDATE_INIT_MESSAGE, entity.getClass().getSimpleName(), entity.getId(), entity));
        if (!repository.existsById(entity.getId())) {
            String message = format(ENTITY_NOT_EXIST_MESSAGE, entity.getClass().getSimpleName(), entity.getId());
            log.error(message);
            throw new EntityNotExistException(message);
        }
        try {
            entity.generateDisplayName();
            E updatedEntity = repository.update(entity);
            log.trace(format(UPDATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), entity.getId(), updatedEntity));
            return updatedEntity;
        }
        catch (OptimisticLockException ex) {
            throw new OptimisticLockingFailureException(format(VERSION_INVALID_MESSAGE, entity.getId()));
        }
    }

    @Override
    @Transactional(readOnly = true)
    public E findById(UUID id) {
        String searchParameter = "id";
        log.trace(format(FIND_INIT_MESSAGE, searchParameter, id));
        if (repository.existsById(id)) {
            Optional<E> baseEntity = repository.findById(id);
            log.trace(format(FIND_FINISH_MESSAGE, searchParameter, id, baseEntity.get()));
            return baseEntity.get();
        } else {
            String errorMessage = format(FIND_INVALID_MESSAGE, searchParameter, id);
            log.error(errorMessage);
            throw new EntityNotExistException(errorMessage);
        }
    }

    @Override
    @Transactional(readOnly = true)
    public Page<E> findAllByFilter(Specification<E> specification, Pageable pageable) {
        log.trace(format(ALL_FIND_BY_FILTER_INIT_MESSAGE, pageable.getPageSize()));
        Page<E> page = repository.findAll(specification, pageable);
        log.trace(format(ALL_FIND_BY_FILTER_FINISH_MESSAGE, page.getTotalElements(), pageable.getPageNumber()));
        return page;
    }

    @Override
    @Transactional(readOnly = true)
    public Page<E> findAll(Pageable pageable) {
        log.trace(format(ALL_FIND_INIT_MESSAGE, pageable.getPageSize()));
        Page<E> page = repository.findAll(pageable);
        log.trace(format(ALL_FIND_FINISH_MESSAGE, page.getTotalElements(), pageable.getPageNumber()));
        return page;
    }

    @Override
    @Transactional(readOnly = true)
    public List<E> findAll() {
        return (List<E>) repository.findAll();
    }

}
