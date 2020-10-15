package ru.itterminal.botdesk.aau.repository;

import java.util.Optional;

import org.springframework.stereotype.Repository;

import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.commons.repository.CustomizedParentEntityRepository;

@Repository
public interface RoleRepository extends CustomizedParentEntityRepository<Role> {
    Optional<Role> getByName(String name);
}
