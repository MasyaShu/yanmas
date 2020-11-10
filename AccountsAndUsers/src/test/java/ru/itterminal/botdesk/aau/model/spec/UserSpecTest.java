package ru.itterminal.botdesk.aau.model.spec;

import static org.junit.jupiter.api.Assertions.assertEquals;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.repository.UserRepository;
import ru.itterminal.botdesk.aau.repository.UserRepositoryTestConfig;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {UserRepositoryTestConfig.class, UserSpec.class})
@Sql({"/create-user-test.sql"})
class UserSpecTest {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    UserSpec spec;

    private static final String USER_1_EMAIL_EXIST = "m@m.ru";
    private static final String ALL_USERS_PHONE_EXIST = "211-15-15";
    private static final String ALL_USERS_COMMENT_EXIST = "comment comment";
    private static final String EMAIL_NOT_EXIST = "n@n.ru";
    private static final String PHONE_NOT_EXIST = "12345";
    private static final String COMMENT_NOT_EXIST = "comment not exist";
    private static final String FIRST_NAME_NOT_EXIST = "firstNameNotExist";
    private static final String SECOND_NAME_NOT_EXIST = "secondNameNotExist";
    private static final String USER_1_FIRST_NAME_EXIST = "firstName1";
    private static final String USER_1_SECOND_NAME_EXIST = "secondName1";
    private static final String USER_3_OUT_ID_EXIST = "outId468431";
    private static final UUID ACCOUNT_1_ID = UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de");
    private static final UUID ACCOUNT_2_ID = UUID.fromString("bcf98101-2a22-42bf-94cc-c900b50a0b69");
    private static final UUID GROUP_1_ID = UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7");
    private static final UUID GROUP_2_ID = UUID.fromString("99f6b488-7687-4451-b8a1-9fbeb2a3efec");
    private static final UUID ROLE_ACCOUNT_OWNER_ID = UUID.fromString("ba99ce38-1611-4a81-adc9-3a779d58bbfe");
    private static final UUID ROLE_ADMIN_ID = UUID.fromString("607f04b1-f5f9-4f20-9c6f-501c32d773c0");
    private static final UUID ROLE_AUTHOR_ID = UUID.fromString("933f20bf-9262-47bb-83d2-0ca55bbbd3fd");
    private final Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "firstName"));
    private Page<User> foundUsers;

    @SuppressWarnings("deprecation")
    @BeforeAll
    void setUpBeforeAll() {
        Role roleAccountOwner = Role
                .builder()
                .name(Roles.ACCOUNT_OWNER.toString())
                .weight(3)
                .build();
        roleAccountOwner.setId(UUID.fromString("ba99ce38-1611-4a81-adc9-3a779d58bbfe"));
        roleAccountOwner.setDeleted(false);
        roleAccountOwner.setVersion(0);

        Role roleNotExistInDatabase = Role
                .builder()
                .name(Roles.ACCOUNT_OWNER.toString())
                .weight(3)
                .build();
        roleNotExistInDatabase.setId(UUID.randomUUID());
        roleNotExistInDatabase.setDeleted(false);
        roleNotExistInDatabase.setVersion(0);
    }

    @Test
    void getUserByEmailSpec_shouldGetOneUser_whenEmailExistInDatabase() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserByEmailSpec(USER_1_EMAIL_EXIST.toUpperCase()));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(USER_1_EMAIL_EXIST, foundUsers.getContent().get(0).getEmail());
    }

    @Test
    void getUserByEmailSpec_shouldGetEmptyList_whenEmailNotExistInDatabase() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserByEmailSpec(EMAIL_NOT_EXIST.toUpperCase()));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().isEmpty());
    }

    @Test
    void getUserByFirstNameSpec_shouldGetOneUser_whenUserExistInDatabaseWithPassedFirstName() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserByFirstNameSpec(USER_1_FIRST_NAME_EXIST.toUpperCase()));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(USER_1_FIRST_NAME_EXIST, foundUsers.getContent().get(0).getFirstName());
    }

    @Test
    void getUserByFirstNameSpec_shouldTwoUsers_whenFirstNameIsEmpty() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserByFirstNameSpec(""));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(2, foundUsers.getContent().size());
    }

    @Test
    void getUserByFirstNameSpec_shouldGetEmptyList_whenUserNotExistInDatabaseWithPassedSecondName() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserByFirstNameSpec(FIRST_NAME_NOT_EXIST.toUpperCase()));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().isEmpty());
    }

    @Test
    void getUserBySecondNameSpec_shouldGetOneUser_whenUserExistInDatabaseWithPassedSecondName() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserBySecondNameSpec(USER_1_SECOND_NAME_EXIST.toUpperCase()));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(USER_1_SECOND_NAME_EXIST, foundUsers.getContent().get(0).getSecondName());
    }

    @Test
    void getUserBySecondNameSpec_shouldTwoUsers_whenSecondNameIsEmpty() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserBySecondNameSpec(""));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(2, foundUsers.getContent().size());
    }

    @Test
    void getUserBySecondNameSpec_shouldGetEmptyList_whenUserNotExistInDatabaseWithPassedSecondName() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserBySecondNameSpec(SECOND_NAME_NOT_EXIST.toUpperCase()));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().isEmpty());
    }

    @Test
    void getUserByPhoneSpec_shouldGetFiveUsers_whenFiveUserExistInDatabaseWithPassedPhone() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserByPhoneSpec(ALL_USERS_PHONE_EXIST.toUpperCase()));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(3, foundUsers.getContent().size());
    }

    @Test
    void getUserByPhoneSpec_shouldGetTwoUsers_whenPhoneIsEmpty() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserByPhoneSpec(""));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(2, foundUsers.getContent().size());
    }

    @Test
    void getUserByPhoneSpec_shouldGetEmptyList_whenUserNotExistInDatabaseWithPassedPhone() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserByPhoneSpec(PHONE_NOT_EXIST.toUpperCase()));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().isEmpty());
    }

    @Test
    void getUserByCommentSpec_shouldGetFiveUsers_whenFiveUserExistInDatabaseWithPassedComment() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserByCommentSpec(ALL_USERS_COMMENT_EXIST.toUpperCase()));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(3, foundUsers.getContent().size());
    }

    @Test
    void getUserByCommentSpec_shouldGetTwoUsers_whenCommentIsEmpty() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserByCommentSpec(""));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(2, foundUsers.getContent().size());
    }

    @Test
    void getUserByCommentSpec_shouldGetEmptyList_whenUserNotExistInDatabaseWithPassedComment() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserByCommentSpec(COMMENT_NOT_EXIST.toUpperCase()));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().isEmpty());
    }

    @Test
    void getUserByIsArchivedSpec_shouldGetFiveUsers_whenFiveUserExistInDatabaseWithIsArchivedIsFalse() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserByIsArchivedSpec(false));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(5, foundUsers.getContent().size());
    }

    @Test
    void getUserByIsArchivedSpec_shouldGetEmptyList_whenUserNotExistInDatabaseWithIsArchivedIsTrue() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserByIsArchivedSpec(true));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().isEmpty());
    }

    @Test
    void getEntityByAccountSpec_shouldGetFourUsers_whenAccount1() {
        Specification<User> userSpecification = Specification
                .where(spec.getEntityByAccountSpec(ACCOUNT_1_ID));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(4, foundUsers.getContent().size());
    }

    @Test
    void getEntityByAccountSpec_shouldGetOneUser_whenAccount2() {
        Specification<User> userSpecification = Specification
                .where(spec.getEntityByAccountSpec(ACCOUNT_2_ID));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(1, foundUsers.getContent().size());
    }

    @Test
    void getEntityByAccountSpec_shouldGetFiveUsers_whenAccount1AndAccount2() {
        Specification<User> userSpecification = Specification
                .where(spec.getEntityByAccountSpec(ACCOUNT_2_ID))
                .or(spec.getEntityByAccountSpec(ACCOUNT_1_ID));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(5, foundUsers.getContent().size());
    }

    @Test
    void getEntityByAccountSpec_shouldGetEmptyList_whenAccountIdIsNotExistInDatabase() {
        Specification<User> userSpecification = Specification
                .where(spec.getEntityByAccountSpec(UUID.randomUUID()));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.isEmpty());
    }

    @Test
    void getUserByListOfGroupsSpec_shouldGetFourUsers_whenGroup1() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserByListOfGroupsSpec(List.of(GROUP_1_ID)));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(4, foundUsers.getContent().size());
    }

    @Test
    void getUserByListOfGroupsSpec_shouldGetOneUser_whenGroup2() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserByListOfGroupsSpec(List.of(GROUP_2_ID)));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(1, foundUsers.getContent().size());
    }

    @Test
    void getUserByListOfGroupsSpec_shouldGetFiveUsers_whenArrayOfGroup1AndGroup2() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserByListOfGroupsSpec(List.of(GROUP_1_ID, GROUP_2_ID)));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(5, foundUsers.getContent().size());
    }

    @Test
    void getUserByListOfGroupsSpec_shouldGetEmptyList_whenGroupIsNotExistInDatabase() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserByListOfGroupsSpec(List.of(UUID.randomUUID())));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.isEmpty());
    }

    @Test
    void getUserByListOfRolesSpec_shouldGetFiveUsers_whenArrayOfRoles() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserByListOfRolesSpec(List.of(ROLE_ACCOUNT_OWNER_ID, ROLE_AUTHOR_ID)));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(5, foundUsers.getContent().size());
    }

    @Test
    void getUserByListOfRolesSpec_shouldGetOneUser_whenRoleIsAccountOwner() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserByListOfRolesSpec(List.of(ROLE_ACCOUNT_OWNER_ID)));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(1, foundUsers.getContent().size());
    }

    @Test
    void getUserByListOfRolesSpec_shouldGetEmpty_whenRoleIsAdmin() {
        Specification<User> userSpecification = Specification
                .where(spec.getUserByListOfRolesSpec(List.of(ROLE_ADMIN_ID)));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().isEmpty());
    }

    @Test
    void getEntityByDeletedSpec_shouldGetEmptyList_whenDeletedIsTrue() {
        Specification<User> userSpecification = Specification
                .where(spec.getEntityByDeletedSpec(BaseFilterDto.FilterByDeleted.fromString("true")));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().isEmpty());
    }

    @Test
    void getEntityByDeletedSpec_shouldGetFiveUsers_whenDeletedIsFalse() {
        Specification<User> userSpecification = Specification
                .where(spec.getEntityByDeletedSpec(BaseFilterDto.FilterByDeleted.fromString("false")));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(5, foundUsers.getContent().size());
    }

    @Test
    void getEntityByOutIdSpec_shouldGetFourUsers_whenOutIdIsEmpty() {
        Specification<User> userSpecification = Specification
                .where(spec.getEntityByOutIdSpec(""));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(4, foundUsers.getContent().size());
    }

    @Test
    void getEntityByOutIdSpec_shouldGetOneUsers_whenOutIdIsExist() {
        Specification<User> userSpecification = Specification
                .where(spec.getEntityByOutIdSpec(USER_3_OUT_ID_EXIST));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(1, foundUsers.getContent().size());
    }
}