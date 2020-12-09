package ru.itterminal.botdesk.tickets.repository;

import org.springframework.stereotype.Repository;
import ru.itterminal.botdesk.commons.repository.CustomizedParentEntityRepository;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.projection.TicketTypesUniqueFields;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface TicketTypeRepository extends CustomizedParentEntityRepository<TicketType> {

    List<TicketTypesUniqueFields> getByNameAndAccount_IdAndIdNot(String name, UUID accountId, UUID id);

    Optional<TicketType> getByNameAndAccount_Id(String name, UUID accountId);

    Optional<TicketType> getByIdAndAccount_Id(UUID id, UUID accountId);

}
