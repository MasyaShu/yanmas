package ru.itterminal.yanmas.aau.service.impl;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.yanmas.aau.model.Account;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.model.dto.AccountCreateDto;
import ru.itterminal.yanmas.aau.repository.AccountRepository;
import ru.itterminal.yanmas.aau.service.validator.AccountOperationValidator;
import ru.itterminal.yanmas.commons.service.crud.impl.CrudServiceImpl;

import java.util.UUID;

@Slf4j
@Service
@RequiredArgsConstructor
public class AccountServiceImpl extends CrudServiceImpl<Account, AccountOperationValidator, AccountRepository> {

    private final UserServiceImpl userService;
    private final GroupServiceImpl groupService;
    private final RoleServiceImpl roleService;

    private static final String START_CREATE_NEW_ACCOUNT = "Start create a new account {}";
    private static final String FINISH_CREATE_NEW_ACCOUNT = "Finish create a new account with name {}";

    @Transactional
    public Account create(AccountCreateDto accountDto) {
        log.trace(START_CREATE_NEW_ACCOUNT, accountDto);
        validator.checkUniqueness(accountDto.getEmailAccountOwner());
        Account account = Account.builder()
                .name(accountDto.getName())
                .id(UUID.randomUUID())
                .build();
        account.generateDisplayName();
        Account createdAccount = repository.create(account);
        Group groupAccountOwner = Group.builder()
                .account(account)
                .name(accountDto.getNameGroupAccountOwner())
                .isInner(true)
                .isDeprecated(false)
                .build();
        Group createdGroup = groupService.create(groupAccountOwner);
        User user = User.builder()
                .email(accountDto.getEmailAccountOwner())
                .password(accountDto.getPasswordAccountOwner())
                .role(roleService.getAccountOwnerRole())
                .account(account)
                .group(createdGroup)
                .build();
        userService.create(user);
        log.trace(FINISH_CREATE_NEW_ACCOUNT, accountDto);
        return createdAccount;
    }
}
