package ru.itterminal.botdesk.tickets.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.stereotype.Repository;

import ru.itterminal.botdesk.commons.repository.EntityRepositoryWithAccount;
import ru.itterminal.botdesk.tickets.model.SettingsAccessToTicketViaTicketTypes;

@Repository
public interface SettingsAccessToTicketViaTicketTypesRepository
        extends EntityRepositoryWithAccount<SettingsAccessToTicketViaTicketTypes> {

    SettingsAccessToTicketViaTicketTypes getByAccount_IdAndGroupIsNullAndUserIsNullAndDeletedIsFalse(
            UUID accountId
    );

    SettingsAccessToTicketViaTicketTypes getByAccount_IdAndGroup_IdAndUserIsNullAndDeletedIsFalse(
            UUID accountId,
            UUID groupId
    );

    SettingsAccessToTicketViaTicketTypes getByAccount_IdAndGroup_IdAndUser_IdAndDeletedIsFalse(
            UUID accountId,
            UUID groupId,
            UUID userId
    );

    List<SettingsAccessToTicketViaTicketTypes> findAllByAccount_IdAndGroup_IdAndUser_IdAndIdNot(
            UUID accountId,
            UUID groupId,
            UUID userId,
            UUID id);
}
