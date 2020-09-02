package ru.itterminal.botdesk.aau.repository;

import org.springframework.stereotype.Repository;

import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.commons.repository.CustomizedParentEntityRepository;

@Repository
public interface UserRepository extends CustomizedParentEntityRepository<Role> {
}
