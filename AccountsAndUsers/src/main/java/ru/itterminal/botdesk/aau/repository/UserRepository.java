package ru.itterminal.botdesk.aau.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.stereotype.Repository;

import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.projection.UserUniqueFields;
import ru.itterminal.botdesk.commons.repository.CustomizedParentEntityRepository;

@Repository
public interface UserRepository extends CustomizedParentEntityRepository<User> {
    List<UserUniqueFields> getByEmailAndIdNot(String email, UUID id);
}
