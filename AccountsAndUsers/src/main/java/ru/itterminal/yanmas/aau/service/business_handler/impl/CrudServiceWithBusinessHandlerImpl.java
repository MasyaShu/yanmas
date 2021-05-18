package ru.itterminal.yanmas.aau.service.business_handler.impl;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.ifErrorsNotEmptyThrowLogicalValidationException;

import java.util.List;
import java.util.UUID;

import javax.persistence.OptimisticLockException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.CrudServiceWithBusinessHandler;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.aau.service.expansion_spec.ExpansionSpec;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.aau.util.ReflectionHelper;
import ru.itterminal.yanmas.commons.exception.EntityNotExistException;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.commons.repository.EntityRepositoryWithAccount;

@SuppressWarnings({"SpringJavaAutowiredFieldsWarningInspection"})
@Service
public abstract class CrudServiceWithBusinessHandlerImpl<
        E extends BaseEntity,
        B extends EntityBusinessHandler<E>,
        R extends EntityRepositoryWithAccount<E>>
        implements CrudServiceWithBusinessHandler<E> {

    @Autowired
    @Lazy
    protected List<EntityValidator<E>> validators;

    @Autowired
    @Lazy
    protected List<ExpansionSpec<E>> expansionSpecs;

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
        entity.setId(UUID.randomUUID());
        businessHandler.beforeCreate(entity, currentUser);
        checkAccessBeforeCreate(entity, currentUser);
        logicalValidationBeforeCreate(entity);
        entity.generateDisplayName();
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
        specification = expansionSpec(specification, currentUser);
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
        if (currentUser != null) {
            checkAccessBeforeRead(currentUser);
            var foundEntity = repository.findByIdAndAccountId(id, currentUser.getAccount().getId()).orElseThrow(
                    () -> new EntityNotExistException(
                            format(NOT_FOUND_ENTITY_BY_EMAIL, id)
                    )
            );
            checkAccessBeforeRead(foundEntity, currentUser);
            return foundEntity;
        } else {
            return null;
        }
    }

    @Transactional(readOnly = true)
    public E findById(UUID id) {
        return repository.findById(id).orElseThrow(
                () -> new EntityNotExistException(
                        format(NOT_FOUND_ENTITY_BY_EMAIL, id)
                )
        );
    }

    protected void checkAccessBeforeCreate(User currentUser) {
        if (validators != null && validators.size() != 0) { //NOSONAR
            for (EntityValidator<E> validator : validators) {
                validator.checkAccessBeforeCreate(currentUser);
            }
        }
    }

    protected void checkAccessBeforeCreate(E entity, User currentUser) {
        if (validators != null && validators.size() != 0) { //NOSONAR
            for (EntityValidator<E> validator : validators) {
                validator.checkAccessBeforeCreate(entity, currentUser);
            }
        }
    }

    protected void checkAccessBeforeUpdate(User currentUser) {
        if (validators != null && validators.size() != 0) { //NOSONAR
            for (EntityValidator<E> validator : validators) {
                validator.checkAccessBeforeUpdate(currentUser);
            }
        }
    }

    protected void checkAccessBeforeUpdate(E entity, User currentUser) {
        if (validators != null && validators.size() != 0) { //NOSONAR
            for (EntityValidator<E> validator : validators) {
                validator.checkAccessBeforeUpdate(entity, currentUser);
            }
        }
    }

    protected void checkAccessBeforeRead(User currentUser) {
        if (validators != null && validators.size() != 0) { //NOSONAR
            for (EntityValidator<E> validator : validators) {
                validator.checkAccessBeforeRead(currentUser);
            }
        }
    }

    protected void checkAccessBeforeRead(E entity, User currentUser) {
        if (validators != null && validators.size() != 0) { //NOSONAR
            for (EntityValidator<E> validator : validators) {
                validator.checkAccessBeforeRead(entity, currentUser);
            }
        }
    }

    protected void logicalValidationBeforeCreate(E entity) {
        if (validators != null && validators.size() != 0) { //NOSONAR
            var errors = createMapForLogicalErrors();
            for (EntityValidator<E> validator : validators) {
                validator.logicalValidationBeforeCreate(entity, errors);
            }
            ifErrorsNotEmptyThrowLogicalValidationException(errors);
        }
    }

    protected void logicalValidationBeforeUpdate(E entity) {
        if (validators != null && validators.size() != 0) { //NOSONAR
            var errors = createMapForLogicalErrors();
            for (EntityValidator<E> validator : validators) {
                validator.logicalValidationBeforeUpdate(entity, errors);
            }
            ifErrorsNotEmptyThrowLogicalValidationException(errors);
        }
    }

    protected Specification<E>  expansionSpec(Specification<E> specification, User currentUser) {
        if (expansionSpecs != null && expansionSpecs.size() != 0) { //NOSONAR
            for (ExpansionSpec<E> item : expansionSpecs) {
                specification = specification.and(item.expansionSpec(specification, currentUser));
            }
        }
        return specification;
    }

}
