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

    Optional<User> getByEmail(String email);

    Integer countUserByRole_NameAndAccount_Id(String roleName, UUID accountId); //NOSONAR

    Integer countUserByRole_NameAndAccount_IdAndIdNot(String roleName, UUID accountId, UUID userId); //NOSONAR
}
