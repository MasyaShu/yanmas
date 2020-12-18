package ru.itterminal.botdesk.commons.service.impl;

import static java.lang.String.format;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.commons.repository.EntityRepositoryWithAccount;
import ru.itterminal.botdesk.commons.service.CrudServiceWithAccount;
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
public abstract class CrudServiceWithAccountImpl<
        E extends BaseEntity,
        V extends OperationValidator<E>,
        R extends EntityRepositoryWithAccount<E>>
        extends CrudServiceImpl<E, V, R>
        implements CrudServiceWithAccount<E> {

    @Transactional(readOnly = true)
    @Override
    public List<E> findAllByAccountId(UUID accountId) {
        return repository.findAllByAccountId(accountId);
    }

    @Transactional(readOnly = true)
    @Override
    public List<E> findAllByAccountIdAndListId(UUID accountId, List<UUID> listId) {
        return repository.findAllByAccountIdAndListId(accountId, listId);
    }

    @Transactional(readOnly = true)
    @Override
    public E findByIdAndAccountId(UUID id, UUID accountId) {
        var searchParameter = "id and accountId";
        log.trace(format(FIND_INIT_MESSAGE_WITH_ACCOUNT, searchParameter, id, accountId));
        if (repository.existsById(id)) {
            Optional<E> baseEntity = repository.findByIdAndAccountId(id, accountId);
            log.trace(format(FIND_FINISH_MESSAGE, searchParameter, id, baseEntity.get()));
            return baseEntity.get();
        } else {
            String errorMessage = format(FIND_INVALID_MESSAGE_WITH_ACCOUNT, searchParameter, id, accountId);
            log.error(errorMessage);
            throw new EntityNotExistException(errorMessage);
        }
    }
}
