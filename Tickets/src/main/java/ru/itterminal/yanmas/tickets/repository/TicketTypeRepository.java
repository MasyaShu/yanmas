package ru.itterminal.yanmas.tickets.repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.stereotype.Repository;

import ru.itterminal.yanmas.commons.repository.EntityRepositoryWithAccount;
import ru.itterminal.yanmas.tickets.model.TicketType;
import ru.itterminal.yanmas.tickets.model.projection.TicketTypeUniqueFields;

@Repository
public interface TicketTypeRepository extends EntityRepositoryWithAccount<TicketType> {

    List<TicketTypeUniqueFields> getByNameAndAccount_IdAndIdNot(String name, UUID accountId, UUID id);

    Optional<TicketType> getByNameAndAccount_Id(String name, UUID accountId);

    Optional<TicketType> getByIdAndAccount_Id(UUID id, UUID accountId);

    Optional<TicketType> getByIsPredefinedForNewTicketTrueAndAccount_Id(UUID accountId);

}
