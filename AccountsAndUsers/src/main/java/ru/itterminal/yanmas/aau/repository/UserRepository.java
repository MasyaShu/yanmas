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

    // TODO must be deleted after big refactoring
    List<User> findAllByRoleAndIdNot(Role role, UUID id);

    // TODO must be deleted after big refactoring
    List<User> findAllByRoleAndAccount_IdAndIdNot(Role role, UUID accountId, UUID id); //NOSONAR

    // TODO must be deleted after big refactoring
    List<User> findAllByRoleAndAccount_Id(Role role, UUID accountId); //NOSONAR

    Optional<User> getByEmail(String email);

    List<User> findAllByRole(Role role);

    Integer countUserByRole_Name(String roleName); //NOSONAR

    Integer countUserByRole_NameAndIdNot(String roleName, UUID id); //NOSONAR
}
