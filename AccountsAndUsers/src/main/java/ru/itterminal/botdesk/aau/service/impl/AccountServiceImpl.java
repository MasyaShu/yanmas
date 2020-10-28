package ru.itterminal.botdesk.aau.service.impl;

import static java.lang.String.format;

import java.security.Principal;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.dto.AccountCreateDto;
import ru.itterminal.botdesk.aau.repository.AccountRepository;
import ru.itterminal.botdesk.aau.service.validator.AccountOperationValidator;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceImpl;
import ru.itterminal.botdesk.jwt.JwtUser;

@Slf4j
@Service
@Transactional
public class AccountServiceImpl extends CrudServiceImpl<Account, AccountOperationValidator, AccountRepository> {

    private UserServiceImpl userService;
    private GroupServiceImpl groupService;
    private RoleServiceImpl roleService;

    @Autowired
    public AccountServiceImpl(UserServiceImpl userService,
            GroupServiceImpl groupService, RoleServiceImpl roleService) {
        this.userService = userService;
        this.groupService = groupService;
        this.roleService = roleService;
    }

    private static final String START_CREATE_NEW_ACCOUNT = "Start create a new account {}";
    private static final String FINISH_CREATE_NEW_ACCOUNT = "Finish create a new account with name {}";
    private static final String START_UPDATE_NEW_ACCOUNT = "Start update an account: {}";
    private static final String FINISH_UPDATE_NEW_ACCOUNT = "Finish update an account: {}";
    private static final String START_FIND_AN_ACCOUNT_BY_AUTHENTICATED_USER = "Start find an account by authenticated user: {}";
    private static final String FINISH_FIND_AN_ACCOUNT_BY_AUTHENTICATED_USER = "Finish find an account by "
            + "authenticated user: {}";

    public Account create(AccountCreateDto accountDto) {
        log.trace(START_CREATE_NEW_ACCOUNT, accountDto);
        validator.checkUniqueness(accountDto.getEmailAccountOwner());
        Account account = Account.builder()
                .name(accountDto.getName())
                .build();
        account.setId(UUID.randomUUID());
        account.setDeleted(false);
        Account createdAccount = repository.create(account);
        Group groupAccountOwner =  Group.builder()
                .account(account)
                .name(accountDto.getNameGroupAccountOwner())
                .isInner(true)
                .build();
        groupService.create(groupAccountOwner);
        User user = User.builder()
                .email(accountDto.getEmailAccountOwner())
                .password(accountDto.getPasswordAccountOwner())
                .role(roleService.getAccountOwnerRole())
                .account(account)
                .ownGroup(groupAccountOwner)
                .build();
        userService.create(user);
        log.trace(FINISH_CREATE_NEW_ACCOUNT, accountDto);
        return createdAccount;
    }

    @Override
    public Account update (Account account) {
        log.trace(START_FIND_AN_ACCOUNT_BY_AUTHENTICATED_USER, account);
        validator.beforeUpdate(account);
        Account accountFromDatabase = super.findById(account.getId());
        try {
            Account updatedAccount = repository.update(account);
            log.trace(FINISH_UPDATE_NEW_ACCOUNT, account);
            return updatedAccount;
        }
        catch (ObjectOptimisticLockingFailureException ex) {
            throw new OptimisticLockingFailureException(format(VERSION_INVALID_MESSAGE, account.getId()));
        }
    }

    public Account findByAuthenticatedUser(Principal principal) {
        log.trace(START_FIND_AN_ACCOUNT_BY_AUTHENTICATED_USER, principal);
        JwtUser jwtUser = (JwtUser) principal;
        Account foundAccount = findById(jwtUser.getAccountId());
        log.trace(FINISH_FIND_AN_ACCOUNT_BY_AUTHENTICATED_USER, foundAccount);
        return foundAccount;
    }
}
