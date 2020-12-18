package ru.itterminal.botdesk.aau.repository;

import org.springframework.stereotype.Repository;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.projection.GroupUniqueFields;
import ru.itterminal.botdesk.commons.repository.EntityRepositoryWithAccount;

import java.util.List;
import java.util.UUID;

@Repository
public interface GroupRepository extends EntityRepositoryWithAccount<Group> {

    List<GroupUniqueFields> getByNameAndIsInnerAndAccount_IdAndIdNot(String name, Boolean isInner, UUID accountId, UUID id);
}
