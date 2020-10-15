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

    Optional<User> getByEmail(String email);

    Optional<User> getByIdAndAccount_Id(UUID id, UUID accountId);

    List<User> findAllByRolesAndIdNot(Role role, UUID id);

    List<User> findAllByRoles(Role role);
}
