package ru.itterminal.botdesk.aau.repository;

import org.springframework.stereotype.Repository;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.projection.GroupUniqueFields;
import ru.itterminal.botdesk.commons.repository.CustomizedParentEntityRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface GroupRepository extends CustomizedParentEntityRepository<Group> {

    List<GroupUniqueFields> getByNameAndAccount_IdAndIdNot(String name, UUID accountId, UUID id);

    Optional<Group> getByNameAndAccount_Id(String name, UUID accountId);

    Optional<Group> getByIdAndAccount_Id(UUID id, UUID accountId);

    Optional<Group> getByIdAndAccount_IdAAndId(UUID id, UUID accountId, UUID jwtUserGroupId);

}
