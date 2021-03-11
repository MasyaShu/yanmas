package ru.itterminal.botdesk.aau.service.impl;

import static java.lang.String.format;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

import javax.persistence.OptimisticLockException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.service.CrudServiceWithAccount;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.commons.repository.EntityRepositoryWithAccount;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceImpl;
import ru.itterminal.botdesk.commons.service.validator.OperationValidator;
import ru.itterminal.botdesk.security.jwt.JwtUserBuilder;

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
public abstract class CrudServiceWithAccountImpl<
        E extends BaseEntity,
        V extends OperationValidator<E>,
        R extends EntityRepositoryWithAccount<E>>
        extends CrudServiceImpl<E, V, R>
        implements CrudServiceWithAccount<E> {

    @SuppressWarnings("SpringJavaAutowiredFieldsWarningInspection")
    @Autowired
    private JwtUserBuilder jwtUserBuilder;

    @Transactional(readOnly = true)
    @Override
    public List<E> findAllByAccountId() {
        return repository.findAllByAccountId(jwtUserBuilder.getJwtUser().getAccountId());
    }

    @Transactional(readOnly = true)
    @Override
    public List<E> findAllByAccountIdAndListId(List<UUID> listId) {
        if (listId != null && !listId.isEmpty()) {
            return repository.findAllByAccountIdAndListId(jwtUserBuilder.getJwtUser().getAccountId(), listId);
        }
        return Collections.emptyList();
    }

    @Transactional(readOnly = true)
    @Override
    public E findByIdAndAccountId(UUID id) {
        var searchParameter = "id and accountId";
        var accountId = jwtUserBuilder.getJwtUser().getAccountId();
        log.trace(format(FIND_INIT_MESSAGE_WITH_ACCOUNT, searchParameter, id, accountId));
        if (repository.existsById(id)) {
            return repository.findByIdAndAccountId(id, accountId).orElseThrow(
                    () -> {
                        String errorMessage = format(FIND_INVALID_MESSAGE_WITH_ACCOUNT, searchParameter, id, accountId);
                        log.error(errorMessage);
                        throw new EntityNotExistException(errorMessage);
                    }
            );
        } else {
            String errorMessage = format(FIND_INVALID_MESSAGE_WITH_ACCOUNT, searchParameter, id, accountId);
            log.error(errorMessage);
            throw new EntityNotExistException(errorMessage);
        }
    }

    //TODO rewrite create

    @SuppressWarnings("DuplicatedCode")
    @Transactional
    @Override
    public E update(E entity) {
        validator.beforeUpdate(entity);
        log.trace(format(UPDATE_INIT_MESSAGE, entity.getClass().getSimpleName(), entity.getId(), entity));
        var accountId = jwtUserBuilder.getJwtUser().getAccountId();
        repository.findByIdAndAccountId(entity.getId(), accountId);
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
}
