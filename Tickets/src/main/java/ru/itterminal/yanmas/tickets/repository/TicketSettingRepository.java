package ru.itterminal.yanmas.tickets.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.stereotype.Repository;

import ru.itterminal.yanmas.commons.repository.EntityRepositoryWithAccount;
import ru.itterminal.yanmas.tickets.model.TicketSetting;

@Repository
public interface TicketSettingRepository extends EntityRepositoryWithAccount<TicketSetting> {

    TicketSetting getByAccount_IdAndGroupIsNullAndAuthorIsNullAndDeletedIsFalse(UUID accountId);

    TicketSetting getByAccount_IdAndGroup_IdAndAuthorIsNullAndDeletedIsFalse(UUID accountId, UUID groupId);

    TicketSetting getByAccount_IdAndGroup_IdAndAuthor_IdAndDeletedIsFalse(UUID accountId, UUID groupId, UUID authorId);

    List<TicketSetting> findAllByAccount_IdAndGroup_IdAndAuthor_IdAndIdNot(
            UUID accountId,
            UUID groupId,
            UUID authorId,
            UUID id);

    List<TicketSetting> findAllByAccount_IdAndGroup_IdAndAuthor_Id(
            UUID accountId,
            UUID groupId,
            UUID authorId);

}
