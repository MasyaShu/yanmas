package ru.itterminal.botdesk.aau.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import ru.itterminal.botdesk.aau.model.spec.GroupSpec;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {GroupRepositoryTestConfig.class, GroupSpec.class, GroupRepository.class})
@Sql({"/create-user-test.sql"})
class GroupRepositoryTest {

    @Autowired
    private GroupRepository groupRepository;

    @Autowired
    GroupSpec spec;

    private static final String EXIST_NAME = "groupName1";
    private static final String NOT_EXIST_NAME = "groupName4";
    private static final UUID EXIST_ID = UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7");
    private static final UUID NOT_EXIST_ID = UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e8");
    private static final UUID ACCOUNT_1_ID = UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de");
    private static final UUID ACCOUNT_ID_NOT_EXIST = UUID.fromString("3ee38c7d-df86-40a7-8805-f75649f15cd1");

    @Test
    void getByNameAndAccount_IdAndIdNot_shouldGetEmptyList_whenNameNotExistAndIdNotExistInDatabase() {
        assertTrue(
                groupRepository.getByNameAndAccount_IdAndIdNot(NOT_EXIST_NAME, ACCOUNT_1_ID, NOT_EXIST_ID).isEmpty());
    }

    @Test
    void getByNameAndAccount_IdAndIdNot_shouldGetEmptyList_whenNameNotExistAndIdExistInDatabase() {
        assertTrue(groupRepository.getByNameAndAccount_IdAndIdNot(NOT_EXIST_NAME, ACCOUNT_1_ID, EXIST_ID).isEmpty());
    }

    @Test
    void getByNameAndAccount_IdAndIdNot_shouldGetNotNull_whenNameExistAndIdNotExistInDatabase() {
        String name = groupRepository
                .getByNameAndAccount_IdAndIdNot(EXIST_NAME, ACCOUNT_1_ID, NOT_EXIST_ID).get(0).getName();
        assertEquals(EXIST_NAME, name);
    }

    @Test
    void getByNameAndAccount_IdAndIdNot_shouldGetEmptyList_whenNameExistAndIdExistInDatabase() {
        assertTrue(groupRepository.getByNameAndAccount_IdAndIdNot(EXIST_NAME, ACCOUNT_1_ID, EXIST_ID).isEmpty());
    }

    @Test
    void getByNameAndAccount_Id_shouldGetEmptyList_whenNameNotExistAndAccountIdExist() {
        assertTrue(groupRepository.getByNameAndAccount_Id(NOT_EXIST_NAME, ACCOUNT_1_ID).isEmpty());
    }

    @Test
    void getByNameAndAccount_Id_shouldGetNotNull_whenNameExistAndAccountIdExist() {
        assertEquals(EXIST_NAME, groupRepository.getByNameAndAccount_Id(EXIST_NAME, ACCOUNT_1_ID).get().getName());
    }

    @Test
    void getByNameAndAccount_Id_shouldGetEmptyList_whenNameExistAndAccountIdNotExist() {
        assertTrue(groupRepository.getByNameAndAccount_Id(NOT_EXIST_NAME, ACCOUNT_ID_NOT_EXIST).isEmpty());
    }

    @Test
    void getByIdAndAccount_Id_shouldGetEmptyList_whenGroupIdNotExistAndAccountIdExist() {
        assertTrue(groupRepository.getByIdAndAccount_Id(NOT_EXIST_ID, ACCOUNT_1_ID).isEmpty());
    }

    @Test
    void getByIdAndAccount_Id_shouldGetNotNull_whenGroupIdExistAndAccountIdExist() {
        assertEquals(EXIST_NAME, groupRepository.getByIdAndAccount_Id(EXIST_ID, ACCOUNT_1_ID).get().getName());
    }

    @Test
    void getByIdAndAccount_Id_shouldGetEmptyList_whenGroupIdExistAndAccountIdNotExist() {
        assertTrue(groupRepository.getByIdAndAccount_Id(NOT_EXIST_ID, ACCOUNT_ID_NOT_EXIST).isEmpty());
    }
}