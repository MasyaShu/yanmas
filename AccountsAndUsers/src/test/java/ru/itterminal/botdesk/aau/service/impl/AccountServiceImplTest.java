package ru.itterminal.botdesk.aau.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.dto.AccountCreateDto;
import ru.itterminal.botdesk.aau.repository.AccountRepository;
import ru.itterminal.botdesk.aau.service.validator.AccountOperationValidator;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.service.CrudService;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {AccountServiceImpl.class})
class AccountServiceImplTest {

    @MockBean
    UserServiceImpl userService;

    @MockBean
    GroupServiceImpl groupService;

    @MockBean
    RoleServiceImpl roleService;

    @MockBean
    AccountRepository repository;

    @MockBean
    AccountOperationValidator validator;

    @Autowired
    private AccountServiceImpl service;

    private Account account;
    private Group group;
    private User user;
    private AccountCreateDto accountDto;
    private final Role ROLE_ACCOUNT_OWNER = Role.builder().name(Roles.ACCOUNT_OWNER.toString()).build();
    public static final String ACCOUNT_NAME = "name of test account";
    public static final String EMAIL_ACCOUNT_OWNER = "m@m.ru";
    public static final String PASSWORD_ACCOUNT_OWNER = "12345";
    public static final String GROUP_NAME_ACCOUNT_OWNER = "test name of group";
    public static final UUID USER_ID = UUID.fromString("9d2c3bc2-b60d-47a9-884d-da4c094d9cfb");

    @BeforeEach
    void setUpBeforeEach() {
        account = Account
                .builder()
                .name(ACCOUNT_NAME)
                .build();
        account.setId(UUID.fromString(TestSecurityConfig.ACCOUNT_1_ID));
        group = Group
                .builder()
                .name(GROUP_NAME_ACCOUNT_OWNER)
                .build();
        group.setId(UUID.fromString(TestSecurityConfig.GROUP_1_ID));
        user = User
                .builder()
                .email(EMAIL_ACCOUNT_OWNER)
                .build();
        user.setId(USER_ID);
        accountDto = new AccountCreateDto();
        accountDto.setName(ACCOUNT_NAME);
        accountDto.setEmailAccountOwner(EMAIL_ACCOUNT_OWNER);
        accountDto.setPasswordAccountOwner(PASSWORD_ACCOUNT_OWNER);
        accountDto.setNameGroupAccountOwner(GROUP_NAME_ACCOUNT_OWNER);
    }

    @Test
    void create_shouldCreateAccount_whenPassedValidData() {
        when(validator.checkUniqueness(anyString())).thenReturn(true);
        when(repository.create(any())).thenReturn(account);
        when(groupService.create(any())).thenReturn(group);
        when(userService.create(any())).thenReturn(user);
        when(roleService.getAccountOwnerRole()).thenReturn(ROLE_ACCOUNT_OWNER);
        Account createdAccount = service.create(accountDto);
        assertEquals(account, createdAccount);
        verify(validator, times(1)).checkUniqueness(anyString());
        verify(repository, times(1)).create(any());
        verify(groupService, times(1)).create(any());
        verify(userService, times(1)).create(any());
        verify(roleService, times(1)).getAccountOwnerRole();
    }
}