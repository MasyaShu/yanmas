package ru.itterminal.botdesk.tickets.repository;

import org.springframework.stereotype.Repository;
import ru.itterminal.botdesk.commons.repository.CustomizedParentEntityRepository;
import ru.itterminal.botdesk.tickets.model.TicketTypes;
import ru.itterminal.botdesk.tickets.model.projection.TicketTypesUniqueFields;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface TicketTypesRepository extends CustomizedParentEntityRepository<TicketTypes> {

    List<TicketTypesUniqueFields> getByNameAndAccount_IdAndIdNot(String name, UUID accountId, UUID id);

    Optional<TicketTypes> getByNameAndAccount_Id(String name, UUID accountId);

    Optional<TicketTypes> getByIdAndAccount_Id(UUID id, UUID accountId);

}
