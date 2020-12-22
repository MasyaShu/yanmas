package ru.itterminal.botdesk.aau.service.impl;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.dto.AccountCreateDto;
import ru.itterminal.botdesk.aau.repository.AccountRepository;
import ru.itterminal.botdesk.aau.service.validator.AccountOperationValidator;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceImpl;

import java.util.UUID;

@Slf4j
@Service
@Transactional
public class AccountServiceImpl extends CrudServiceImpl<Account, AccountOperationValidator, AccountRepository> {

    private final UserServiceImpl userService;
    private final GroupServiceImpl groupService;
    private final RoleServiceImpl roleService;

    @Autowired
    public AccountServiceImpl(UserServiceImpl userService,
                              GroupServiceImpl groupService, RoleServiceImpl roleService) {
        this.userService = userService;
        this.groupService = groupService;
        this.roleService = roleService;
    }

    private static final String START_CREATE_NEW_ACCOUNT = "Start create a new account {}";
    private static final String FINISH_CREATE_NEW_ACCOUNT = "Finish create a new account with name {}";

    public Account create(AccountCreateDto accountDto) {
        log.trace(START_CREATE_NEW_ACCOUNT, accountDto);
        validator.checkUniqueness(accountDto.getEmailAccountOwner());
        Account account = Account.builder()
                .name(accountDto.getName())
                .id(UUID.randomUUID())
                .deleted(false)
                .build();
        Account createdAccount = repository.create(account);
        Group groupAccountOwner = Group.builder()
                .account(account)
                .name(accountDto.getNameGroupAccountOwner())
                .isInner(true)
                .isDeprecated(false)
                .deleted(false)
                .build();
        groupService.create(groupAccountOwner);
        User user = User.builder()
                .email(accountDto.getEmailAccountOwner())
                .password(accountDto.getPasswordAccountOwner())
                .role(roleService.getAccountOwnerRole())
                .account(account)
                .group(groupAccountOwner)
                .build();
        userService.create(user);
        log.trace(FINISH_CREATE_NEW_ACCOUNT, accountDto);
        return createdAccount;
    }
}
