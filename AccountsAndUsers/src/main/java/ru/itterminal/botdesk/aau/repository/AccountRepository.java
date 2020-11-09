package ru.itterminal.botdesk.aau.repository;

import org.springframework.stereotype.Repository;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.repository.CustomizedParentEntityRepository;

@Repository
public interface AccountRepository extends CustomizedParentEntityRepository<Account> {
}
