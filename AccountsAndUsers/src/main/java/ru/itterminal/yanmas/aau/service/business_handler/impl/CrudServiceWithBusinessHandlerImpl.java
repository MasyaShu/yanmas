package ru.itterminal.yanmas.aau.service.business_handler.impl;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.ifErrorsNotEmptyThrowLogicalValidationException;

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

import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.CrudServiceWithBusinessHandler;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.aau.util.ReflectionHelper;
import ru.itterminal.yanmas.commons.exception.EntityNotExistException;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.commons.repository.EntityRepositoryWithAccount;

@SuppressWarnings({"SpringJavaAutowiredFieldsWarningInspection", "SpringJavaInjectionPointsAutowiringInspection"})
@Service
public abstract class CrudServiceWithBusinessHandlerImpl<
        E extends BaseEntity,
        B extends EntityBusinessHandler<E>,
        R extends EntityRepositoryWithAccount<E>>
        implements CrudServiceWithBusinessHandler<E> {

    protected List<EntityValidator<E>> validators;

    @Autowired
    protected B businessHandler;

    @Autowired
    protected ReflectionHelper reflectionHelper;

    @Autowired
    protected R repository;

    public static final String NOT_FOUND_ENTITY_BY_EMAIL = "Not found entity by id: %s";

    @Override
    @Transactional
    public E create(E entity, User currentUser) {
        checkAccessBeforeCreate(currentUser);
        reflectionHelper.settingNestedObjectsIntoEntity(entity, currentUser);
        businessHandler.beforeCreate(entity, currentUser);
        checkAccessBeforeCreate(entity, currentUser);
        entity.setId(UUID.randomUUID());
        entity.generateDisplayName();
        logicalValidationBeforeCreate(entity);
        var createdEntity = repository.create(entity);
        businessHandler.afterCreate(createdEntity, currentUser);
        return createdEntity;
    }

    @Override
    @Transactional
    public E update(E entity, User currentUser) {
        checkAccessBeforeUpdate(currentUser);
        reflectionHelper.settingNestedObjectsIntoEntity(entity, currentUser);
        findByIdAndAccountId(entity.getId(), currentUser);
        businessHandler.beforeUpdate(entity, currentUser);
        checkAccessBeforeUpdate(entity, currentUser);
        logicalValidationBeforeUpdate(entity);
        try {
            entity.generateDisplayName();
            var updatedEntity = repository.update(entity);
            businessHandler.afterUpdate(updatedEntity, currentUser);
            return updatedEntity;
        }
        catch (OptimisticLockException ex) {
            throw new OptimisticLockingFailureException(format(VERSION_INVALID_MESSAGE, entity.getId()));
        }
    }

    @Override
    @Transactional(readOnly = true)
    public Page<E> findAllByFilter(Specification<E> specification, Pageable pageable, User currentUser) {
        checkAccessBeforeRead(currentUser);
        businessHandler.beforeFindAllByFilter(specification, currentUser);
        return repository.findAll(specification, pageable);
    }

    @Override
    @Transactional(readOnly = true)
    public List<E> findAllByAccountId(User currentUser) {
        checkAccessBeforeRead(currentUser);
        return repository.findAllByAccountId(currentUser.getAccount().getId());
    }

    @Override
    @Transactional(readOnly = true)
    public List<E> findAllByAccountIdAndListId(List<UUID> listId, User currentUser) {
        checkAccessBeforeRead(currentUser);
        return repository.findAllByAccountIdAndListId(currentUser.getAccount().getId(), listId);
    }

    @Override
    @Transactional(readOnly = true)
    public E findByIdAndAccountId(UUID id, User currentUser) {
        checkAccessBeforeRead(currentUser);
        var foundEntity =  repository.findByIdAndAccountId(id, currentUser.getAccount().getId()).orElseThrow(
                () -> new EntityNotExistException(
                        format(NOT_FOUND_ENTITY_BY_EMAIL, id)
                )
        );
        checkAccessBeforeRead(foundEntity, currentUser);
        return foundEntity;
    }

    private void  checkAccessBeforeCreate(User currentUser) {
        for (EntityValidator<E> validator : validators) {
            validator.checkAccessBeforeCreate(currentUser);
        }
    }

    private void  checkAccessBeforeCreate(E entity, User currentUser) {
        for (EntityValidator<E> validator : validators) {
            validator.checkAccessBeforeCreate(entity, currentUser);
        }
    }

    private void  checkAccessBeforeUpdate(User currentUser) {
        for (EntityValidator<E> validator : validators) {
            validator.checkAccessBeforeUpdate(currentUser);
        }
    }

    private void  checkAccessBeforeUpdate(E entity, User currentUser) {
        for (EntityValidator<E> validator : validators) {
            validator.checkAccessBeforeUpdate(entity, currentUser);
        }
    }

    private void  checkAccessBeforeRead(User currentUser) {
        for (EntityValidator<E> validator : validators) {
            validator.checkAccessBeforeRead(currentUser);
        }
    }

    private void  checkAccessBeforeRead(E entity, User currentUser) {
        for (EntityValidator<E> validator : validators) {
            validator.checkAccessBeforeRead(entity, currentUser);
        }
    }

    private void  logicalValidationBeforeCreate(E entity) {
        var errors = createMapForLogicalErrors();
        for (EntityValidator<E> validator : validators) {
            validator.logicalValidationBeforeCreate(entity, errors);
        }
        ifErrorsNotEmptyThrowLogicalValidationException(errors);
    }

    private void  logicalValidationBeforeUpdate(E entity) {
        var errors = createMapForLogicalErrors();
        for (EntityValidator<E> validator : validators) {
            validator.logicalValidationBeforeUpdate(entity, errors);
        }
        ifErrorsNotEmptyThrowLogicalValidationException(errors);
    }

}
