package ru.itterminal.yanmas.aau.service.impl;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.yanmas.aau.model.Account;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.model.dto.AccountCreateDto;
import ru.itterminal.yanmas.aau.repository.AccountRepository;
import ru.itterminal.yanmas.aau.service.validator.account.AccountOperationValidator;
import ru.itterminal.yanmas.commons.service.crud.impl.CrudServiceImpl;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class AccountServiceImpl extends CrudServiceImpl<Account, AccountOperationValidator, AccountRepository> {

    private final UserServiceImpl userService;
    private final GroupServiceImpl groupService;
    private final RoleServiceImpl roleService;

    @Transactional
    public Account create(AccountCreateDto accountDto) {
        validator.checkUniqueness(accountDto.getEmailAccountOwner());
        var account = Account.builder()
                .name(accountDto.getName())
                .id(UUID.randomUUID())
                .build();
        account.generateDisplayName();
        var createdAccount = repository.create(account);
        var groupAccountOwner = Group.builder()
                .account(account)
                .name(accountDto.getNameGroupAccountOwner())
                .isInner(true)
                .isDeprecated(false)
                .build();
        var createdGroupAccountOwner = groupService.create(groupAccountOwner, null);
        var userAccountOwner = User.builder()
                .email(accountDto.getEmailAccountOwner())
                .password(accountDto.getPasswordAccountOwner())
                .role(roleService.getAccountOwnerRole())
                .account(account)
                .group(createdGroupAccountOwner)
                .build();
        userService.create(userAccountOwner, null);
        return createdAccount;
    }
}
