package ru.itterminal.botdesk.aau.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
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
import org.springframework.context.annotation.Import;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.repository.GroupRepository;
import ru.itterminal.botdesk.aau.service.validator.GroupOperationValidator;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {GroupServiceImpl.class})
@Import(TestSecurityConfig.class)
@TestPropertySource(locations = "/application.properties")
@ActiveProfiles("Test")
class GroupServiceImplTest {

    @MockBean
    private GroupRepository repository;

    @MockBean
    private GroupOperationValidator validator;

    @Autowired
    private GroupServiceImpl service;

    private Group group;
    private Group groupInDataBase;
    private static final String EXIST_NAME = "groupName1";
    private static final UUID ACCOUNT_1_ID = UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de");
    private static final UUID GROUP_ID = UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7");

    @BeforeEach
    void setUpBeforeEach() {
        Account account = new Account();
        account.setId(ACCOUNT_1_ID);
        group = Group
                .builder()
                .name(EXIST_NAME)
                .account(account)
                .isInner(false)
                .build();
        group.setId(GROUP_ID);
        groupInDataBase = Group
                .builder()
                .name(EXIST_NAME)
                .account(account)
                .isInner(true)
                .build();
        group.setId(GROUP_ID);
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserIsNull() {
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(null));
        verify(repository, times(0)).getByNameAndIsInnerAndAccount_IdAndIdNot(any(), any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserNameIsNull() {
        group.setName(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(group));
        verify(repository, times(0)).getByNameAndIsInnerAndAccount_IdAndIdNot(any(), any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserAccountIsNull() {
        group.setAccount(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(group));
        verify(repository, times(0)).getByNameAndIsInnerAndAccount_IdAndIdNot(any(), any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserIdIsNull() {
        group.setId(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(group));
        verify(repository, times(0)).getByNameAndIsInnerAndAccount_IdAndIdNot(any(), any(), any(), any());
    }

    @Test
    void update_shouldUpdateUser_whenPassedValidData() {
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(repository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(group));
        when(repository.update(any())).thenReturn(group);
        Group createdGroup = service.update(group);
        assertEquals(createdGroup, group);
        verify(validator, times(1)).beforeUpdate(any());
        verify(repository, times(1)).findByIdAndAccountId(any(), any());
        verify(repository, times(1)).update(any());
    }

    @Test
    void update_shouldUpdateUser_whenPassedValidDataAndIsInnerNotDatabase() {
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(repository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(groupInDataBase));
        when(repository.update(any())).thenReturn(group);
        Group createdGroup = service.update(group);
        assertSame(groupInDataBase.getIsInner(), createdGroup.getIsInner());
        verify(validator, times(1)).beforeUpdate(any());
        verify(repository, times(1)).findByIdAndAccountId(any(), any());
        verify(repository, times(1)).update(any());
    }
}