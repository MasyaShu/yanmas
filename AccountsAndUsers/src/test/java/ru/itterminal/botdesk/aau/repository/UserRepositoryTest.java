package ru.itterminal.botdesk.aau.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;

@SuppressWarnings("deprecation")
@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {UserRepositoryTestConfig.class})
@Sql({"/create-user-test.sql"})
class UserRepositoryTest {

    @Autowired
    private UserRepository userRepository;

    private static Role roleAccountOwner = null;
    private static Role roleNotExistInDatabase = null;
    private static final String USER_1_EMAIL_EXIST = "m@m.ru";
    private static final UUID USER_1_ID_EXIST = UUID.fromString("d592facb-e6ee-4801-8310-9c7708eb6e6c");
    private static final UUID USER_1_OWN_GROUP_ID_EXIST = UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7");
    private static final UUID USER_1_ACCOUNT_ID_EXIST
            = UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de");
    private static final UUID ID_NOT_EXIST = UUID.fromString("cb9e8816-7bed-4bb6-b3ea-3aa0eee247b6");
    private static final String EMAIL_NOT_EXIST = "n@n.ru";

    @BeforeAll
    void setUpBeforeAll() {
        roleAccountOwner = Role
                .builder()
                .name(Roles.ACCOUNT_OWNER.toString())
                .weight(3)
                .build();
        roleAccountOwner.setId(UUID.fromString("ba99ce38-1611-4a81-adc9-3a779d58bbfe"));
        roleAccountOwner.setDeleted(false);
        roleAccountOwner.setVersion(0);

        roleNotExistInDatabase = Role
                .builder()
                .name(Roles.ACCOUNT_OWNER.toString())
                .weight(3)
                .build();
        roleNotExistInDatabase.setId(UUID.randomUUID());
        roleNotExistInDatabase.setDeleted(false);
        roleNotExistInDatabase.setVersion(0);
    }

    @Test
    void getByEmailAndIdNot_shouldGetEmptyList_whenEmailNotExistAndIdNotExistInDatabase() {
        assertTrue(userRepository.getByEmailAndIdNot(EMAIL_NOT_EXIST, ID_NOT_EXIST).isEmpty());
    }

    @Test
    void getByEmailAndIdNot_shouldGetEmptyList_whenEmailNotExistAndIdExistInDatabase() {
        assertTrue(userRepository.getByEmailAndIdNot(EMAIL_NOT_EXIST, USER_1_ID_EXIST).isEmpty());
    }

    @Test
    void getByEmailAndIdNot_shouldGetOneUserUniqueFields_whenEmailExistAndIdNotExistInDatabase() {
        assertEquals(USER_1_EMAIL_EXIST,
                userRepository.getByEmailAndIdNot(USER_1_EMAIL_EXIST, ID_NOT_EXIST).get(0).getEmail());
    }

    @Test
    void getByEmailAndIdNot_shouldGetEmptyList_whenEmailExistAndIdExistInDatabase() {
        assertTrue(userRepository.getByEmailAndIdNot(USER_1_EMAIL_EXIST, USER_1_ID_EXIST).isEmpty());
    }

    @Test
    void findAllByRoleAndIdNot_shouldGetEmpty_whenAccountOwnerExistAndIdEquals() {
        List<User> foundUsers;
        foundUsers = userRepository.findAllByRoleAndIdNot(roleAccountOwner, USER_1_ID_EXIST);
        assertTrue(foundUsers.isEmpty());
    }

    @Test
    void findAllByRoleAndIdNot_shouldGetOneUser_whenRoleAccountOwnerExistAndIdNotEquals() {
        List<User> foundUsers;
        foundUsers = userRepository.findAllByRoleAndIdNot(roleAccountOwner, UUID.randomUUID());
        assertEquals(1, foundUsers.size());
    }

    @Test
    void findAllByRoleAndAccountId_shouldGetOneUser_whenRoleAccountOwnerExistAndIdEquals() {
        List<User> foundUsers;
        foundUsers = userRepository.findAllByRoleAndAccount_Id(roleAccountOwner, USER_1_ACCOUNT_ID_EXIST);
        assertEquals(USER_1_ID_EXIST, foundUsers.get(0).getId());
    }

    @Test
    void findAllByRoleAndAccountId_shouldGetEmptyList_whenRoleNotExistAndIdEquals() {
        List<User> foundUsers;
        foundUsers = userRepository.findAllByRoleAndAccount_Id(roleNotExistInDatabase, USER_1_ACCOUNT_ID_EXIST);
        assertTrue(foundUsers.isEmpty());
    }

    @Test
    void getByIdAndAccountIdAndOwnGroupId_shouldGetOneUser_whenUserInDatabaseHasAllPassedParameters() {
        User user = userRepository.getByIdAndAccount_IdAndOwnGroup_Id(USER_1_ID_EXIST, USER_1_ACCOUNT_ID_EXIST,
                USER_1_OWN_GROUP_ID_EXIST).get();
        assertNotNull(user);
        assertEquals(USER_1_ID_EXIST, user.getId());
        assertEquals(USER_1_ACCOUNT_ID_EXIST, user.getAccount().getId());
        assertEquals(USER_1_OWN_GROUP_ID_EXIST, user.getOwnGroup().getId());
    }

}