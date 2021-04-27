package ru.itterminal.yanmas.aau.service.business_handler;

import java.util.List;
import java.util.UUID;

import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.CrudServiceWithAccount;
import ru.itterminal.yanmas.commons.model.BaseEntity;

@SuppressWarnings("unused")
public interface CrudServiceWithBusinessHandler<E extends BaseEntity> extends CrudServiceWithAccount<E> {

    List<E> findAllByAccountId(User currentUser);

    List<E> findAllByAccountIdAndListId(List<UUID> listId, User currentUser);

    List<E> findAllByAccountIdAndListId(UUID accountId, List<UUID> listId, User currentUser);

    E findByIdAndAccountId(UUID id, User currentUser);

    E findByIdAndAccountId(UUID id, UUID accountId, User currentUser);
}
