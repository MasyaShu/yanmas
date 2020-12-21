package ru.itterminal.botdesk.aau.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.Test;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.test.AccountTestHelper;
import ru.itterminal.botdesk.aau.model.dto.AccountDto;

class TestAccountHelperTest {

    AccountTestHelper accountHelper = new AccountTestHelper();

    @Test
    void getRandomValidEntity() {
        Account account = accountHelper.getRandomValidEntity();
        assertTrue(account.getName().length() <= 128);
    }

    @Test
    void getRandomInvalidEntity() {
        Account account = accountHelper.getRandomInvalidEntity();
        assertTrue(account.getName().length() > 128);
    }

    @Test
    void getRandomValidEntityList() {
        List<Account> accountList = accountHelper.getRandomValidEntityList(3);
        for (Account account : accountList) {
            assertTrue(account.getName().length() <= 128);
        }
    }

    @Test
    void getRandomInvalidEntityList() {
        List<Account> accountList = accountHelper.getRandomInvalidEntityList(3);
        for (Account account : accountList) {
            assertTrue(account.getName().length() > 128);
        }
    }

    @Test
    void getPredefinedValidEntityList() {
        List<Account> accountList = accountHelper.getPredefinedValidEntityList();
        assertEquals(2, accountList.size());
    }

}