package ru.itterminal.yanmas.aau.repository;

import org.springframework.stereotype.Repository;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.projection.GroupUniqueFields;
import ru.itterminal.yanmas.commons.repository.EntityRepositoryWithAccount;

import java.util.List;
import java.util.UUID;

@Repository
public interface GroupRepository extends EntityRepositoryWithAccount<Group> {

    List<GroupUniqueFields> getByNameAndIsInnerAndAccount_IdAndIdNot(String name, Boolean isInner, UUID accountId, UUID id);

    List<GroupUniqueFields> getByNameAndIsInnerAndAccount_Id(String name, Boolean isInner, UUID accountId);

}
