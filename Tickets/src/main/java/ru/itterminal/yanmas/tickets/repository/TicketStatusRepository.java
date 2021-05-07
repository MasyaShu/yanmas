package ru.itterminal.yanmas.tickets.repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.stereotype.Repository;

import ru.itterminal.yanmas.commons.repository.EntityRepositoryWithAccount;
import ru.itterminal.yanmas.tickets.model.TicketStatus;
import ru.itterminal.yanmas.tickets.model.projection.TicketStatusUniqueFields;

@Repository
public interface TicketStatusRepository extends EntityRepositoryWithAccount<TicketStatus> {

    List<TicketStatusUniqueFields> getByNameAndAccount_IdAndIdNot(String name, UUID accountId, UUID id);

    List<TicketStatusUniqueFields> getByNameAndAccount_Id(String name, UUID accountId);

    Optional<TicketStatus> getByIdAndAccount_Id(UUID id, UUID accountId);

    Optional<TicketStatus> getByIsStartedPredefinedTrueAndAccount_Id(UUID accountId);

    Optional<TicketStatus> getByIsCanceledPredefinedTrueAndAccount_Id(UUID accountId);

    Optional<TicketStatus> getByIsReopenedPredefinedTrueAndAccount_Id(UUID accountId);

    Optional<TicketStatus> getByIsFinishedPredefinedTrueAndAccount_Id(UUID accountId);
}
