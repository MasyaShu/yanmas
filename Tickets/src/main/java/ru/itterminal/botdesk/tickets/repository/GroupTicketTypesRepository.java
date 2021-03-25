package ru.itterminal.botdesk.tickets.repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.stereotype.Repository;

import ru.itterminal.botdesk.commons.repository.EntityRepositoryWithAccount;
import ru.itterminal.botdesk.tickets.model.GroupTicketTypes;
import ru.itterminal.botdesk.tickets.model.projection.GroupTicketTypesUniqueFields;

@Repository
public interface GroupTicketTypesRepository extends EntityRepositoryWithAccount<GroupTicketTypes> {

    List<GroupTicketTypesUniqueFields> getByNameAndAccount_IdAndIdNot(String name, UUID accountId, UUID id);

    Optional<GroupTicketTypes> getByNameAndAccount_Id(String name, UUID accountId);

    Optional<GroupTicketTypes> getByIdAndAccount_Id(UUID id, UUID accountId);

}
