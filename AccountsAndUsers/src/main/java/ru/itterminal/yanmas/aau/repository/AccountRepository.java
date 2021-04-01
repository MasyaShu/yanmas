package ru.itterminal.yanmas.aau.repository;

import org.springframework.stereotype.Repository;

import ru.itterminal.yanmas.aau.model.Account;
import ru.itterminal.yanmas.commons.repository.CustomizedParentEntityRepository;

@Repository
public interface AccountRepository extends CustomizedParentEntityRepository<Account> {
}
