package ru.itterminal.botdesk.aau.repository;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import ru.itterminal.botdesk.aau.model.spec.GroupSpec;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

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

    @Test
    void getByNameAndIsInnerAndAccount_IdAndIdNot_shouldGetEmptyList_whenNameNotExistAndIdNotExistInDatabase() {
        assertTrue(
                groupRepository.getByNameAndIsInnerAndAccount_IdAndIdNot(NOT_EXIST_NAME, true, ACCOUNT_1_ID, NOT_EXIST_ID).isEmpty());
    }

    @Test
    void getByNameAndIsInnerAndAccount_IdAndIdNot_shouldGetEmptyList_whenNameNotExistAndIdExistInDatabase() {
        assertTrue(groupRepository.getByNameAndIsInnerAndAccount_IdAndIdNot(NOT_EXIST_NAME, true, ACCOUNT_1_ID, EXIST_ID).isEmpty());
    }

    @Test
    void getByNameAndIsInnerAndAccount_IdAndIdNot_shouldGetEmptyList_whenNameExistIsInnerTrueAndIdExistInDatabase() {
        assertTrue(groupRepository.getByNameAndIsInnerAndAccount_IdAndIdNot(EXIST_NAME, true, ACCOUNT_1_ID, EXIST_ID).isEmpty());
    }

    @Test
    void getByNameAndIsInnerAndAccount_IdAndIdNot_shouldGetEmptyList_whenNameExistAndIdNotExistInDatabase() {
        assertTrue(groupRepository.getByNameAndIsInnerAndAccount_IdAndIdNot(EXIST_NAME, false, ACCOUNT_1_ID, NOT_EXIST_ID).isEmpty());
    }

    @Test
    void getByNameAndIsInnerAndAccount_IdAndIdNot_shouldGetNotNull_whenNameExistIsInnerTrueAndIdNotExistInDatabase() {
        String name = groupRepository
                .getByNameAndIsInnerAndAccount_IdAndIdNot(EXIST_NAME, true, ACCOUNT_1_ID, NOT_EXIST_ID).get(0).getName();
        assertEquals(EXIST_NAME, name);
    }
}