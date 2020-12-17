package ru.itterminal.botdesk.aau.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
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
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;

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
    private static Role roleAuthor = null;
    private static Role roleNotExistInDatabase = null;
    private static final String USER_1_EMAIL_EXIST = "m@m.ru";
    private static final UUID USER_1_ID_EXIST = UUID.fromString("d592facb-e6ee-4801-8310-9c7708eb6e6c");
    private static final UUID USER_1_OWN_GROUP_ID_EXIST = UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7");
    private static final UUID USER_1_ACCOUNT_ID_EXIST
            = UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de");
    private static final UUID USER_2_ACCOUNT_ID_EXIST
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

        roleAuthor = Role
                .builder()
                .name(Roles.AUTHOR.toString())
                .weight(1)
                .build();
        roleAuthor.setId(UUID.fromString("933f20bf-9262-47bb-83d2-0ca55bbbd3fd"));
        roleAuthor.setDeleted(false);
        roleAuthor.setVersion(0);

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
        Assertions.assertEquals(
                USER_1_EMAIL_EXIST,
                userRepository.getByEmailAndIdNot(USER_1_EMAIL_EXIST, ID_NOT_EXIST).get(0).getEmail()
        );
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
        Assertions.assertEquals(USER_1_ID_EXIST, foundUsers.get(0).getId());
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
                                                                      USER_1_OWN_GROUP_ID_EXIST
        ).get();
        assertNotNull(user);
        Assertions.assertEquals(USER_1_ID_EXIST, user.getId());
        Assertions.assertEquals(USER_1_ACCOUNT_ID_EXIST, user.getAccount().getId());
        Assertions.assertEquals(USER_1_OWN_GROUP_ID_EXIST, user.getOwnGroup().getId());
    }

    @Test
    void findAllByRoleAndAccount_IdAndIdNot_shouldGetThreeUsers_whenDatabaseHasThreeUsersWithPassedParameters() {
        List<User> userList = userRepository.findAllByRoleAndAccount_IdAndIdNot(
                roleAuthor, USER_2_ACCOUNT_ID_EXIST, USER_1_ID_EXIST);
        assertEquals(3, userList.size());
    }

    @Test
    void findAllByAccountId_shouldGetFourUsers_whenThereAreFourUsersWithTheSameAccountId() {
        List<User> userList = userRepository.findAllByAccountId(USER_1_ACCOUNT_ID_EXIST);
        assertEquals(4, userList.size());
    }

    @Test
    void findAllByAccountId_shouldGetEmptyList_whenThereAreNotTheSameAccountId() {
        List<User> userList = userRepository.findAllByAccountId(UUID.randomUUID());
        assertTrue(userList.isEmpty());
    }

    @Test
    void findByIdAndAccountId_shouldGetOneUser_whenThereIsOneUserWithTheSameAccountIdAndTheSameId() {
        User user = userRepository.findByIdAndAccountId(USER_1_ID_EXIST, USER_1_ACCOUNT_ID_EXIST).get();
        assertNotNull(user);
    }

    @Test
    void findByIdAndAccountId_shouldGetNull_whenThereIsNotUsersWithTheSameAccountIdAndTheSameId() {
        assertTrue(userRepository.findByIdAndAccountId(UUID.randomUUID(), UUID.randomUUID()).isEmpty());
    }

    @Test
    void findAllByAccountIdAndListId_shouldGetTwoUsers_whenThereAreFourUsersWithTheSameAccountIdAndInListId() {
        var userHelper = new UserTestHelper();
        var predefinedUserList = userHelper.getPredefinedValidEntityList();
        var userList = userRepository.
                findAllByAccountIdAndListId(
                        USER_1_ACCOUNT_ID_EXIST,
                        List.of(
                                predefinedUserList.get(0).getId(),
                                predefinedUserList.get(1).getId()
                        )
                );
        assertEquals(2, userList.size());
    }

    @Test
    void findAllByAccountIdAndListId_shouldGetEmptyList_whenThereAreTheSameAccountIdAndEmptyListId() {
        List<User> userList = userRepository.findAllByAccountIdAndListId(UUID.randomUUID(), Collections.emptyList());
        assertTrue(userList.isEmpty());
    }

}