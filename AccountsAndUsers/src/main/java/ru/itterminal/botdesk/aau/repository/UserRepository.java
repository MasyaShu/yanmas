package ru.itterminal.botdesk.aau.repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.stereotype.Repository;

import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.projection.UserUniqueFields;
import ru.itterminal.botdesk.commons.repository.CustomizedParentEntityRepository;

@Repository
public interface UserRepository extends CustomizedParentEntityRepository<User> {

    List<UserUniqueFields> getByEmailAndIdNot(String email, UUID id);

    List<User> findAllByRoleAndIdNot(Role role, UUID id);

    List<User> findAllByRoleAndAccount_IdAndIdNot(Role role, UUID accountId, UUID id);

    List<User> findAllByRoleAndAccount_Id(Role role, UUID accountId);

    Optional<User> getByEmail(String email);

    Optional<User> getByIdAndAccount_Id(UUID id, UUID accountId);

    Optional<User> getByIdAndAccount_IdAndOwnGroup_Id(UUID id, UUID accountId, UUID ownGroupId);

    List<User> findAllByRole(Role role);
}
