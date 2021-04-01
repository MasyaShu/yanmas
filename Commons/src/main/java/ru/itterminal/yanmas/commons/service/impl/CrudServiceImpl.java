package ru.itterminal.yanmas.commons.service.impl;

import static java.lang.String.format;

import java.util.List;
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
import ru.itterminal.yanmas.commons.exception.EntityNotExistException;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.commons.repository.CustomizedParentEntityRepository;
import ru.itterminal.yanmas.commons.service.CrudService;
import ru.itterminal.yanmas.commons.service.validator.OperationValidator;

@SuppressWarnings({"SpringJavaAutowiredFieldsWarningInspection", "DuplicatedCode"})
@Slf4j
@Service
public abstract class CrudServiceImpl<E extends BaseEntity,
        V extends OperationValidator<E>,
        R extends CustomizedParentEntityRepository<E>>
        implements CrudService<E> {

    @SuppressWarnings("SpringJavaInjectionPointsAutowiringInspection")
    @Autowired
    protected R repository;

    @SuppressWarnings("SpringJavaInjectionPointsAutowiringInspection")
    @Autowired
    protected V validator;

    @Override
    @Transactional
    public E create(E entity) {
        validator.checkAccessBeforeCreate(entity);
        validator.logicalValidationBeforeCreate(entity);
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
    @Transactional
    public E update(E entity) {
        validator.checkAccessBeforeUpdate(entity);
        validator.logicalValidationBeforeUpdate(entity);
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
        } catch (OptimisticLockException ex) {
            throw new OptimisticLockingFailureException(format(VERSION_INVALID_MESSAGE, entity.getId()));
        }
    }

    @Override
    @Transactional(readOnly = true)
    public E findById(UUID id) {
        String searchParameter = "id";
        log.trace(format(FIND_INIT_MESSAGE, searchParameter, id));
        if (repository.existsById(id)) {
            @SuppressWarnings("OptionalGetWithoutIsPresent")
            var foundEntity = repository.findById(id).get(); //NOSONAR
            validator.checkAccessBeforeRead(foundEntity);
            log.trace(format(FIND_FINISH_MESSAGE, searchParameter, id, foundEntity));
            return foundEntity;
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
