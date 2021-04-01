package ru.itterminal.yanmas.aau.repository;

import java.util.Optional;

import org.springframework.stereotype.Repository;

import ru.itterminal.yanmas.aau.model.Role;
import ru.itterminal.yanmas.commons.repository.CustomizedParentEntityRepository;

@Repository
public interface RoleRepository extends CustomizedParentEntityRepository<Role> {
    Optional<Role> getByName(String name);
}
