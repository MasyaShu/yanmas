package ru.itterminal.yanmas.tickets.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.stereotype.Repository;

import ru.itterminal.yanmas.commons.repository.EntityRepositoryWithAccount;
import ru.itterminal.yanmas.tickets.model.SettingsAccessToTicketTypes;

@Repository
public interface SettingsAccessToTicketTypesRepository
        extends EntityRepositoryWithAccount<SettingsAccessToTicketTypes> {

    SettingsAccessToTicketTypes getByAccount_IdAndGroupIsNullAndUserIsNullAndDeletedIsFalse(
            UUID accountId
    );

    SettingsAccessToTicketTypes getByAccount_IdAndGroup_IdAndUserIsNullAndDeletedIsFalse(
            UUID accountId,
            UUID groupId
    );

    SettingsAccessToTicketTypes getByAccount_IdAndGroup_IdAndUser_IdAndDeletedIsFalse(
            UUID accountId,
            UUID groupId,
            UUID userId
    );

    List<SettingsAccessToTicketTypes> findAllByAccount_IdAndGroup_IdAndUser_IdAndIdNot(
            UUID accountId,
            UUID groupId,
            UUID userId,
            UUID id);
}
