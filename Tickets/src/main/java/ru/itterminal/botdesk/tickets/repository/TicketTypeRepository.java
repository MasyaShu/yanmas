package ru.itterminal.botdesk.tickets.repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.stereotype.Repository;

import ru.itterminal.botdesk.commons.repository.EntityRepositoryWithAccount;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.projection.TicketTypeUniqueFields;

@Repository
public interface TicketTypeRepository extends EntityRepositoryWithAccount<TicketType> {

    List<TicketTypeUniqueFields> getByNameAndAccount_IdAndIdNot(String name, UUID accountId, UUID id);

    Optional<TicketType> getByNameAndAccount_Id(String name, UUID accountId);

    Optional<TicketType> getByIdAndAccount_Id(UUID id, UUID accountId);

}
