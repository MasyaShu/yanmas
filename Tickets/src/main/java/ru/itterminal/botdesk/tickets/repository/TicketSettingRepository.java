package ru.itterminal.botdesk.tickets.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import ru.itterminal.botdesk.commons.repository.EntityRepositoryWithAccount;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.projection.TicketSettingUniqueFields;

@Repository
public interface TicketSettingRepository extends EntityRepositoryWithAccount<TicketSetting> {
    @Query(value = "select account_id, group_id, author_id  from ticket_settings where"
            + " account_id = ?1 and group_id=?2 and author_id=?3 and id<>?4", nativeQuery = true)
    List<TicketSettingUniqueFields> findByUniqueFields(UUID accountId, UUID groupId, UUID authorId, UUID id);

    @Query(value = "select account_id, group_id, author_id  from ticket_settings where"
            + " account_id = ?1 and group_id=?2 and author_id=?3 and id<>?4 and deleted<>true", nativeQuery = true)
    List<TicketSettingUniqueFields> findByUniqueFieldsWithoutDeleted
            (UUID accountId, UUID groupId, UUID authorId, UUID id);

}
