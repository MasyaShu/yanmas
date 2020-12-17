package ru.itterminal.botdesk.tickets.repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.stereotype.Repository;

import ru.itterminal.botdesk.commons.repository.EntityRepositoryWithAccount;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.projection.TicketStatusUniqueFields;

@Repository
public interface TicketStatusRepository extends EntityRepositoryWithAccount<TicketStatus> {

    List<TicketStatusUniqueFields> getByNameAndAccount_IdAndIdNot(String name, UUID accountId, UUID id);

    Optional<TicketStatus> getByNameAndAccount_Id(String name, UUID accountId);

    Optional<TicketStatus> getByIdAndAccount_Id(UUID id, UUID accountId);
}
