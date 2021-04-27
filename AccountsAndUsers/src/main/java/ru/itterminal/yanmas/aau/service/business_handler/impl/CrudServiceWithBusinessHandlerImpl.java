package ru.itterminal.yanmas.aau.service.business_handler.impl;

import static java.lang.String.format;

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
import ru.itterminal.yanmas.aau.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.yanmas.aau.service.validator.OperationValidatorWithCurrentUser;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.commons.repository.EntityRepositoryWithAccount;

@SuppressWarnings("SpringJavaAutowiredFieldsWarningInspection")
@Service
public abstract class CrudServiceWithBusinessHandlerImpl<
        E extends BaseEntity,
        V extends OperationValidatorWithCurrentUser<E>,
        B extends EntityBusinessHandler<E>,
        R extends EntityRepositoryWithAccount<E>>
        extends CrudServiceWithAccountImpl<E, V, R>
        implements CrudServiceWithBusinessHandler<E> {

    @Autowired
    protected B businessHandler;

    @Autowired
    protected V validator;

    @Transactional
    public E create(E entity, User currentUser) {
        businessHandler.beforeCreate(entity, currentUser);
        validator.checkAccessBeforeCreate(entity, currentUser);
        validator.logicalValidationBeforeCreate(entity);
        UUID id = UUID.randomUUID();
        entity.setId(id);
        entity.generateDisplayName();
        validator.checkUniqueness(entity);
        E createdEntity;
        createdEntity = repository.create(entity);
        businessHandler.afterCreate(createdEntity, currentUser);
        return createdEntity;
    }

    @Transactional
    public E update(E entity, User currentUser) {
        businessHandler.beforeUpdate(entity, currentUser);
        validator.checkAccessBeforeUpdate(entity, currentUser);
        validator.logicalValidationBeforeUpdate(entity);
        try {
            entity.generateDisplayName();
            E updatedEntity = repository.update(entity);
            businessHandler.afterUpdate(updatedEntity, currentUser);
            return updatedEntity;
        }
        catch (OptimisticLockException ex) {
            throw new OptimisticLockingFailureException(format(VERSION_INVALID_MESSAGE, entity.getId()));
        }
    }

    public Page<E> findAllByFilter(Specification<E> specification, Pageable pageable, User currentUser) {
        businessHandler.beforeFindAllByFilter(specification, currentUser);
        return repository.findAll(specification, pageable);
    }
}
