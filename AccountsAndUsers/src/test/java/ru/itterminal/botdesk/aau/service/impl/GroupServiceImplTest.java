package ru.itterminal.botdesk.aau.service.impl;

import static java.lang.String.format;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl.ENTITY_GROUP_NAME;
import static ru.itterminal.botdesk.commons.util.CommonConstants.NOT_FOUND_ENTITY_BY_ID_AND_ACCOUNT_ID;

import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.repository.GroupRepository;
import ru.itterminal.botdesk.aau.service.validator.GroupOperationValidator;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.config.TestSecurityConfig;

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
    private Account account;
    private static final String EXIST_NAME = "groupName1";
    private static final UUID ACCOUNT_1_ID = UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de");
    private static final UUID GROUP_ID = UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7");
    private static final UUID GROUP_ID_NOT_JWTUSER = UUID.fromString("4d219da4-bcfe-42bd-b095-ceb7b5f4dfa1");

    @BeforeEach
    void setUpBeforeEach() {
        account = new Account();
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
        verify(repository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserNameIsNull() {
        group.setName(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(group));
        verify(repository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserAccountIsNull() {
        group.setAccount(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(group));
        verify(repository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserIdIsNull() {
        group.setId(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(group));
        verify(repository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void update_shouldUpdateUser_whenPassedValidData() {
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(repository.findById(any())).thenReturn(Optional.of(group));
        when(repository.update(any())).thenReturn(group);
        Group createdGroup = service.update(group);
        assertEquals(createdGroup, group);
        verify(validator, times(1)).beforeUpdate(any());
        verify(repository, times(1)).findById(any());
        verify(repository, times(1)).update(any());
    }

    @Test
    void update_shouldUpdateUser_whenPassedValidDataAndIsInnerNotDatabase() {
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(repository.findById(any())).thenReturn(Optional.of(groupInDataBase));
        when(repository.update(any())).thenReturn(group);
        Group createdGroup = service.update(group);
        assertSame(groupInDataBase.getIsInner(), createdGroup.getIsInner());
        verify(validator, times(1)).beforeUpdate(any());
        verify(repository, times(1)).findById(any());
        verify(repository, times(1)).update(any());
    }

    @Test
    void findByIdAndAccountId_shouldGetEntityNotExistException_whenGroupIdIsNull() {
        UUID accountId = account.getId();
        assertThrows(EntityNotExistException.class, () -> service.findByIdAndAccountId(null, accountId));
        verify(repository, times(0)).getByIdAndAccount_Id(any(), any());
    }

    @Test
    void findByIdAndAccountId_shouldGetEntityNotExistException_whenAccountIdIsNull() {
        UUID groupId = group.getId();
        assertThrows(EntityNotExistException.class, () -> service.findByIdAndAccountId(groupId, null));
        verify(repository, times(0)).getByIdAndAccount_Id(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void findByIdAndAccountId_shouldGetEntityNotExistException_whenUserNotInnerGroupAndGroupOwnerNotEqualsFindId() {
        UUID accountId = account.getId();
        Throwable throwable = assertThrows(EntityNotExistException.class,
                () -> service.findByIdAndAccountId(GROUP_ID_NOT_JWTUSER,accountId));
        assertEquals(
                format(NOT_FOUND_ENTITY_BY_ID_AND_ACCOUNT_ID, ENTITY_GROUP_NAME, GROUP_ID_NOT_JWTUSER, accountId),
                throwable.getMessage());
        verify(repository, times(0)).getByIdAndAccount_Id(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void findByIdAndAccountId_shouldGetOneGroup_whenUserNotInnerGroupAndGroupOwnerEqualsFindId() {
        when(repository.getByIdAndAccount_Id(any(), any())).thenReturn(Optional.of(group));
        Group foundGroup = service.findByIdAndAccountId(GROUP_ID, account.getId());
        assertEquals(group, foundGroup);
        verify(repository, times(1)).getByIdAndAccount_Id(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void findByIdAndAccountId_shouldNotGetEntityNotExistException_whenUserInnerGroupAndGroupOwnerEqualsFindId() {
        when(repository.getByIdAndAccount_Id(any(), any())).thenReturn(Optional.of(group));
        service.findByIdAndAccountId(GROUP_ID, account.getId());
        verify(repository, times(1)).getByIdAndAccount_Id(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void findByIdAndAccountId_shouldNotGetEntityNotExistException_whenUserInnerGroupAndGroupOwnerNotEqualsFindId() {
        when(repository.getByIdAndAccount_Id(any(), any())).thenReturn(Optional.of(group));
        service.findByIdAndAccountId(GROUP_ID_NOT_JWTUSER, account.getId());
        verify(repository, times(1)).getByIdAndAccount_Id(any(), any());
    }

}