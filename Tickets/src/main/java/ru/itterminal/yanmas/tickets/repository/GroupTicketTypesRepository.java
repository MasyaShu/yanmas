package ru.itterminal.yanmas.tickets.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.stereotype.Repository;

import ru.itterminal.yanmas.commons.repository.EntityRepositoryWithAccount;
import ru.itterminal.yanmas.tickets.model.GroupTicketTypes;
import ru.itterminal.yanmas.tickets.model.projection.GroupTicketTypesUniqueFields;

@Repository
public interface GroupTicketTypesRepository extends EntityRepositoryWithAccount<GroupTicketTypes> {

    List<GroupTicketTypesUniqueFields> getByNameAndAccount_IdAndIdNot(String name, UUID accountId, UUID id);

    List<GroupTicketTypesUniqueFields> getByNameAndAccount_Id(String name, UUID accountId);

}
