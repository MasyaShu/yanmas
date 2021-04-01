package ru.itterminal.yanmas.aau.repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.stereotype.Repository;

import ru.itterminal.yanmas.aau.model.Role;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.model.projection.UserUniqueFields;
import ru.itterminal.yanmas.commons.repository.EntityRepositoryWithAccount;

@Repository
public interface UserRepository extends EntityRepositoryWithAccount<User> {

    List<UserUniqueFields> getByEmailAndIdNot(String email, UUID id);

    List<User> findAllByRoleAndIdNot(Role role, UUID id);

    List<User> findAllByRoleAndAccount_IdAndIdNot(Role role, UUID accountId, UUID id);

    List<User> findAllByRoleAndAccount_Id(Role role, UUID accountId);

    Optional<User> getByEmail(String email);

    List<User> findAllByRole(Role role);
}
