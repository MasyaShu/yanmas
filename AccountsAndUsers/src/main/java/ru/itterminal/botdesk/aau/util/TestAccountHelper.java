package ru.itterminal.botdesk.aau.util;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.dto.AccountDto;
import ru.itterminal.botdesk.commons.model.TestEntityHelper;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;

public class TestAccountHelper implements TestEntityHelper<Account, AccountDto, BaseFilterDto> {

    private static final String INVALID_ACCOUNT_NAME_REGEX = "[A-Za-z0-9]{129}";

    @Override
    public Account getRandomValidEntity() {
        Account account = Account.builder()
                .name(fakerRU.name().firstName())
                .build();
        setRandomValidPropertiesOfBaseEntity(account);
        return account;
    }

    @Override
    public Account getRandomInvalidEntity() {
        Account account = Account.builder()
                .name(fakerRU.regexify(INVALID_ACCOUNT_NAME_REGEX))
                .build();
        setRandomValidPropertiesOfBaseEntity(account);
        return account;
    }

    @Override
    public List<Account> getRandomValidEntityList(int countEntity) {
        List<Account> accountList = new ArrayList<>();
        for (int i = 0; i < countEntity; i++) {
            accountList.add(getRandomValidEntity());
        }
        return accountList;
    }

    @Override
    public List<Account> getRandomInvalidEntityList(int countEntity) {
        List<Account> accountList = new ArrayList<>();
        for (int i = 0; i < countEntity; i++) {
            accountList.add(getRandomInvalidEntity());
        }
        return accountList;
    }

    @Override
    public List<Account> getPredefinedValidEntityList() {
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
        return List.of(account1, account2);
    }

    @Override
    public List<Account> getPredefinedInvalidEntityList() {
        Account account1 = Account.builder()
                .name(fakerRU.regexify(INVALID_ACCOUNT_NAME_REGEX))
                .build();
        setRandomValidPropertiesOfBaseEntity(account1);
        Account account2 = Account.builder()
                .name(fakerRU.regexify(INVALID_ACCOUNT_NAME_REGEX))
                .build();
        setRandomValidPropertiesOfBaseEntity(account2);
        return List.of(account1, account2);
    }

    @Override
    public AccountDto getRandomValidEntityDto() {
        AccountDto accountDto = AccountDto.builder()
                .name(fakerRU.name().firstName())
                .build();
        setRandomValidPropertiesOfBaseEntityDto(accountDto);
        return accountDto;
    }

    @Override
    public AccountDto getRandomInvalidEntityDto() {
        AccountDto accountDto = AccountDto.builder()
                .name(fakerRU.regexify(INVALID_ACCOUNT_NAME_REGEX))
                .build();
        setRandomValidPropertiesOfBaseEntityDto(accountDto);
        return accountDto;
    }
}
