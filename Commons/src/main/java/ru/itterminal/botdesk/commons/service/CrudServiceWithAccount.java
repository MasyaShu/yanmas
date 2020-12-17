package ru.itterminal.botdesk.commons.service;

import java.util.List;
import java.util.UUID;

import ru.itterminal.botdesk.commons.model.BaseEntity;

@SuppressWarnings("unused")
public interface CrudServiceWithAccount<E extends BaseEntity> {

    String FIND_INIT_MESSAGE_WITH_ACCOUNT = "Search for entity with %s: '%s', '%s'";
    String FIND_INVALID_MESSAGE_WITH_ACCOUNT = "Could not find entity by %s: '%s','%s'";

    List<E> findAllByAccountId(UUID accountId);

    List<E> findAllByAccountIdAndListId(UUID accountId, List<UUID> listId);

    E findByIdAndAccountId(UUID id, UUID accountId);
}
