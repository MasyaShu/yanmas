package ru.itterminal.botdesk.aau.model.test;

import java.util.List;
import java.util.UUID;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.dto.AccountDto;
import ru.itterminal.botdesk.commons.model.EntityTestHelperImpl;

public class AccountTestHelper extends EntityTestHelperImpl<Account, AccountDto, AccountDto> {

    @Override
    public Account getRandomValidEntity() {
        Account account = Account.builder()
                .name(fakerRU.name().firstName())
                .build();
        setRandomValidPropertiesOfBaseEntity(account);
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

}
