package ru.itterminal.yanmas.aau.model.test;

import java.util.List;
import java.util.UUID;

import org.modelmapper.ModelMapper;

import ru.itterminal.yanmas.aau.model.Account;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.model.dto.AccountCreateDto;
import ru.itterminal.yanmas.aau.model.dto.AccountDto;
import ru.itterminal.yanmas.commons.model.EntityTestHelperImpl;

public class AccountTestHelper extends EntityTestHelperImpl<Account, AccountDto, AccountDto> {

    private final ModelMapper modelMapper = new ModelMapper();

    @Override
    public Account getRandomValidEntity() {
        Account account = Account.builder()
                .name(fakerRU.name().firstName())
                .build();
        setRandomValidPropertiesOfBaseEntity(account);
        account.generateDisplayName();
        return account;
    }

    @Override
    public List<Account> setPredefinedValidEntityList() {
        Account account1 = Account.builder()
                .name("accountName1")
                .build();
        setPropertiesOfBaseEntity(
                account1,
                UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de"),
                0,
                false,
                null
        );
        account1.generateDisplayName();
        Account account2 = Account.builder()
                .name("accountName2")
                .build();
        setPropertiesOfBaseEntity(
                account2,
                UUID.fromString("bcf98101-2a22-42bf-94cc-c900b50a0b69"),
                0,
                false,
                null
        );
        account2.generateDisplayName();
        return List.of(account1, account2);
    }

    public AccountCreateDto convertUserToAccountCreateDto(User user) {
        return AccountCreateDto.builder()
                .name(user.getAccount().getName())
                .emailAccountOwner(user.getEmail())
                .passwordAccountOwner(user.getPassword())
                .nameGroupAccountOwner(user.getEmail())
                .build();
    }

    public AccountDto convertAccountToAccountDto(Account account) {
        var accountDto = modelMapper.map(account, AccountDto.class);
        accountDto.setDisplayName(null);
        return accountDto;
    }
}
