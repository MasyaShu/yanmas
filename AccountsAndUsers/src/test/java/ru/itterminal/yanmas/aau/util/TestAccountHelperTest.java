package ru.itterminal.yanmas.aau.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.Test;

import ru.itterminal.yanmas.aau.model.Account;
import ru.itterminal.yanmas.aau.model.test.AccountTestHelper;

class TestAccountHelperTest {

    AccountTestHelper accountHelper = new AccountTestHelper();

    @Test
    void getRandomValidEntity() {
        Account account = accountHelper.getRandomValidEntity();
        assertTrue(account.getName().length() <= 128);
    }

    @Test
    void getRandomValidEntityList() {
        List<Account> accountList = accountHelper.getRandomValidEntityList(3);
        for (Account account : accountList) {
            assertTrue(account.getName().length() <= 128);
        }
    }

    @Test
    void getPredefinedValidEntityList() {
        List<Account> accountList = accountHelper.getPredefinedValidEntityList();
        assertEquals(2, accountList.size());
    }

}