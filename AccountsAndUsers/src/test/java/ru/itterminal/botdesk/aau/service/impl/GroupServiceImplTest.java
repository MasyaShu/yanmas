package ru.itterminal.botdesk.aau.service.impl;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.Language;
import ru.itterminal.botdesk.aau.repository.GroupRepository;
import ru.itterminal.botdesk.aau.service.validator.GroupOperationValidator;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;

import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.when;
import static ru.itterminal.botdesk.config.TestSecurityConfig.GROUP_1_ID;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {GroupServiceImpl.class})
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
    private Account account;
    private static final String EXIST_NAME = "groupName1";
    private static final UUID ACCOUNT_1_ID = UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de");
    private static final UUID GROUP_ID = UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7");

    @BeforeEach
    void setUpBeforeEach() {
        account = new Account().builder()
                .language(Language.RU.toString())
                .build();
        account.setId(ACCOUNT_1_ID);
        group = new Group().builder()
        .name(EXIST_NAME)
        .account(account)
        .isInner(false)
        .build();
        group.setId(GROUP_ID);
        groupInDataBase = new Group().builder()
                .name(EXIST_NAME)
                .account(account)
                .isInner(true)
                .build();
        group.setId(GROUP_ID);
    }

    @Test
    public void findByUniqueFields_shouldGetEntityNotExistException_whenUserIsNull() {
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(null));
        verify(repository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    public void findByUniqueFields_shouldGetEntityNotExistException_whenUserNamelIsNull() {
        group.setName(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(group));
        verify(repository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    public void findByUniqueFields_shouldGetEntityNotExistException_whenUserAccountIsNull() {
        group.setAccount(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(group));
        verify(repository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    public void findByUniqueFields_shouldGetEntityNotExistException_whenUserIdIsNull() {
        group.setId(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(group));
        verify(repository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    public void update_shouldUpdateUser_whenPassedValidData() {
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(repository.findById(any())).thenReturn(Optional.of(group));
        when(repository.update(any())).thenReturn(group);
        Group createdGroup = service.update(group);
        assertTrue(group.equals(createdGroup));
        verify(validator, times(1)).beforeUpdate(any());
        verify(repository, times(1)).findById(any());
        verify(repository, times(1)).update(any());
    }

    @Test
    public void update_shouldUpdateUser_whenPassedValidDataAndIsInnerNotDataBase() {
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(repository.findById(any())).thenReturn(Optional.of(groupInDataBase));
        when(repository.update(any())).thenReturn(group);
        Group createdGroup = service.update(group);
        assertTrue(groupInDataBase.getIsInner() == createdGroup.getIsInner());
        verify(validator, times(1)).beforeUpdate(any());
        verify(repository, times(1)).findById(any());
        verify(repository, times(1)).update(any());
    }

}