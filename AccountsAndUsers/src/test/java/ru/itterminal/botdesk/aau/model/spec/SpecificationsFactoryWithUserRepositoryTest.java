package ru.itterminal.botdesk.aau.model.spec;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;
import static ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;
import static ru.itterminal.botdesk.commons.model.filter.StringFilter.TypeComparisonForStringFilter.IS_EMPTY;
import static ru.itterminal.botdesk.commons.model.filter.StringFilter.TypeComparisonForStringFilter.TEXT_EQUALS;

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
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.dto.UserFilterDtoNew;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.aau.repository.UserRepository;
import ru.itterminal.botdesk.aau.repository.UserRepositoryTestConfig;
import ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter;
import ru.itterminal.botdesk.commons.model.filter.BooleanFilter;
import ru.itterminal.botdesk.commons.model.filter.StringFilter;
import ru.itterminal.botdesk.commons.model.spec.SpecificationsFactory;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {UserRepositoryTestConfig.class, SpecificationsFactory.class})
@Sql({"/create-user-test.sql"})
class SpecificationsFactoryWithUserRepositoryTest {

    private static final String USER_3_EMAIL_EXIST = "m2@m.ru";
    @Autowired
    private UserRepository userRepository;

    @Autowired
    private SpecificationsFactory specificationsFactory;

    private final Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "name"));
    private Page<User> foundUsers;

    private static final String USER_3_OUT_ID_EXIST = "outId468431";
    private static final UUID ROLE_ADMIN_ID = UUID.fromString("607f04b1-f5f9-4f20-9c6f-501c32d773c0");
    private final UserTestHelper userTestHelper = new UserTestHelper();
    private final UUID ACCOUNT_1_ID = userTestHelper.getPredefinedValidEntityList().get(0).getAccount().getId();
    private final UUID ACCOUNT_2_ID = userTestHelper.getPredefinedValidEntityList().get(4).getAccount().getId();
    private final String USER_1_EMAIL_EXIST = userTestHelper.getPredefinedValidEntityList().get(0).getEmail();
    private final String USER_1_NAME_EXIST = userTestHelper.getPredefinedValidEntityList().get(0).getName();
    private final UUID ROLE_ACCOUNT_OWNER_ID = UUID.fromString("ba99ce38-1611-4a81-adc9-3a779d58bbfe");
    private final UUID ROLE_AUTHOR_ID = UUID.fromString("933f20bf-9262-47bb-83d2-0ca55bbbd3fd");

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
    void getUserByEmail_shouldGetOneUser_whenEmailExistInDatabase() {
        var emailFilter = StringFilter.builder()
                .typeComparison(TEXT_EQUALS.toString())
                .value(USER_1_EMAIL_EXIST)
                .build();
        var filterDto = UserFilterDtoNew.builder()
                .email(emailFilter)
                .build();
        var userSpecification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(User.class, filterDto, ACCOUNT_1_ID);
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(USER_1_EMAIL_EXIST, foundUsers.getContent().get(0).getEmail());
    }

    @Test
    void getUserByEmail_shouldGetEmptyList_whenEmailNotExistInDatabase() {
        var emailFilter = StringFilter.builder()
                .typeComparison(TEXT_EQUALS.toString())
                .value(userTestHelper.getRandomValidEntity().getEmail())
                .build();
        var filterDto = UserFilterDtoNew.builder()
                .email(emailFilter)
                .build();
        var userSpecification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(User.class, filterDto, ACCOUNT_1_ID);
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().isEmpty());
    }

    @Test
    void getUserByName_shouldGetOneUser_whenUserExistInDatabaseWithPassedName() {
        var nameFilter = StringFilter.builder()
                .typeComparison(TEXT_EQUALS.toString())
                .value(USER_1_NAME_EXIST)
                .build();
        var filterDto = UserFilterDtoNew.builder()
                .name(nameFilter)
                .build();
        var userSpecification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(User.class, filterDto, ACCOUNT_1_ID);
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(USER_1_NAME_EXIST, foundUsers.getContent().get(0).getName());
    }

    @Test
    void getUserByName_shouldTwoUsers_whenNameIsEmpty() {
        var nameFilter = StringFilter.builder()
                .typeComparison(IS_EMPTY.toString())
                .build();
        var filterDto = UserFilterDtoNew.builder()
                .name(nameFilter)
                .build();
        var userSpecification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(User.class, filterDto, ACCOUNT_1_ID);
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(2, foundUsers.getContent().size());
    }

    @Test
    void getUserByName_shouldGetEmptyList_whenUserNotExistInDatabaseWithPassedSecondName() {
        var nameFilter = StringFilter.builder()
                .typeComparison(TEXT_EQUALS.toString())
                .value(userTestHelper.getRandomValidEntity().getName())
                .build();
        var filterDto = UserFilterDtoNew.builder()
                .name(nameFilter)
                .build();
        var userSpecification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(User.class, filterDto, ACCOUNT_1_ID);
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().isEmpty());
    }


    @Test
    void getUserByPhone_shouldGetTwoUsers_whenTwoUserExistInDatabaseWithPassedPhone() {
        var phoneFilter = StringFilter.builder()
                .typeComparison(TEXT_EQUALS.toString())
                .value(userTestHelper.getPredefinedValidEntityList().get(0).getPhone())
                .build();
        var filterDto = UserFilterDtoNew.builder()
                .phone(phoneFilter)
                .build();
        var userSpecification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(User.class, filterDto, ACCOUNT_1_ID);
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(2, foundUsers.getContent().size());
    }

    @Test
    void getUserByPhone_shouldGetTwoUsers_whenPhoneIsEmpty() {
        var phoneFilter = StringFilter.builder()
                .typeComparison(IS_EMPTY.toString())
                .build();
        var filterDto = UserFilterDtoNew.builder()
                .phone(phoneFilter)
                .build();
        var userSpecification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(User.class, filterDto, ACCOUNT_1_ID);
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(2, foundUsers.getContent().size());
    }

    @Test
    void getUserByPhone_shouldGetEmptyList_whenUserNotExistInDatabaseWithPassedPhone() {
        var phoneFilter = StringFilter.builder()
                .typeComparison(TEXT_EQUALS.toString())
                .value(userTestHelper.getRandomValidEntity().getPhone())
                .build();
        var filterDto = UserFilterDtoNew.builder()
                .phone(phoneFilter)
                .build();
        var userSpecification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(User.class, filterDto, ACCOUNT_1_ID);
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().isEmpty());
    }

    @Test
    void getUserByComment_shouldGetTwoUsers_whenTwoUserExistInDatabaseWithPassedComment() {
        var commentFilter = StringFilter.builder()
                .typeComparison(TEXT_EQUALS.toString())
                .value(userTestHelper.getPredefinedValidEntityList().get(0).getComment())
                .build();
        var filterDto = UserFilterDtoNew.builder()
                .comment(commentFilter)
                .build();
        var userSpecification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(User.class, filterDto, ACCOUNT_1_ID);
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(2, foundUsers.getContent().size());
    }

    @Test
    void getUserByComment_shouldGetTwoUsers_whenCommentIsEmpty() {
        var commentFilter = StringFilter.builder()
                .typeComparison(IS_EMPTY.toString())
                .build();
        var filterDto = UserFilterDtoNew.builder()
                .comment(commentFilter)
                .build();
        var userSpecification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(User.class, filterDto, ACCOUNT_1_ID);
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(2, foundUsers.getContent().size());
    }

    @Test
    void getUserByComment_shouldGetEmptyList_whenUserNotExistInDatabaseWithPassedComment() {
        var commentFilter = StringFilter.builder()
                .typeComparison(TEXT_EQUALS.toString())
                .value(userTestHelper.getRandomValidEntity().getComment())
                .build();
        var filterDto = UserFilterDtoNew.builder()
                .comment(commentFilter)
                .build();
        var userSpecification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(User.class, filterDto, ACCOUNT_1_ID);
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().isEmpty());
    }

    @Test
    void getUserByIsArchived_shouldGetFiveUsers_whenFiveUserExistInDatabaseWithIsArchivedIsFalse() {
        var isArchivedFilter = BooleanFilter.builder()
                .value(false)
                .build();
        var filterDto = UserFilterDtoNew.builder()
                .isArchived(isArchivedFilter)
                .build();
        var userSpecification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(User.class, filterDto, ACCOUNT_1_ID);
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(4, foundUsers.getContent().size());
    }

    @Test
    void getUserByIsArchived_shouldGetEmptyList_whenUserNotExistInDatabaseWithIsArchivedIsTrue() {
        var isArchivedFilter = BooleanFilter.builder()
                .value(true)
                .build();
        var filterDto = UserFilterDtoNew.builder()
                .isArchived(isArchivedFilter)
                .build();
        var userSpecification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(User.class, filterDto, ACCOUNT_1_ID);
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().isEmpty());
    }


    @Test
    void getUserByListOfRoles_shouldGetFourUsers_whenAllRolesOfUsersInPassedList() {
        var roleFilter = BaseEntityFilter.builder()
                .typeComparison(EXIST_IN.toString())
                .listOfIdEntities(List.of(ROLE_ACCOUNT_OWNER_ID, ROLE_AUTHOR_ID))
                .build();
        var filterDto = UserFilterDtoNew.builder()
                .role(roleFilter)
                .build();
        var userSpecification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(User.class, filterDto, ACCOUNT_1_ID);
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(4, foundUsers.getContent().size());
    }

    @Test
    void getUserByListOfRoles_shouldGetOneUser_whenRoleIsAccountOwner() {
        var roleFilter = BaseEntityFilter.builder()
                .typeComparison(EXIST_IN.toString())
                .listOfIdEntities(List.of(ROLE_ACCOUNT_OWNER_ID))
                .build();
        var filterDto = UserFilterDtoNew.builder()
                .role(roleFilter)
                .build();
        var userSpecification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(User.class, filterDto, ACCOUNT_1_ID);
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(1, foundUsers.getContent().size());
    }

    @Test
    void getUserByListOfRoles_shouldGetEmpty_whenRoleIsAdmin() {
        var roleFilter = BaseEntityFilter.builder()
                .typeComparison(EXIST_IN.toString())
                .listOfIdEntities(List.of(ROLE_ADMIN_ID))
                .build();
        var filterDto = UserFilterDtoNew.builder()
                .role(roleFilter)
                .build();
        var userSpecification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(User.class, filterDto, ACCOUNT_1_ID);
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().isEmpty());
    }

    @Test
    void getEntityByDeleted_shouldGetOneUser_whenDeletedIsTrue() {
        var deletedFilter = BooleanFilter.builder()
                .value(true)
                .build();
        var filterDto = UserFilterDtoNew.builder()
                .deleted(deletedFilter)
                .build();
        var userSpecification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(User.class, filterDto, ACCOUNT_2_ID);
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(1, foundUsers.getContent().size());
    }

    @Test
    void getEntityByDeleted_shouldFourUsers_whenDeletedIsFalse() {
        var deletedFilter = BooleanFilter.builder()
                .value(false)
                .build();
        var filterDto = UserFilterDtoNew.builder()
                .deleted(deletedFilter)
                .build();
        var userSpecification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(User.class, filterDto, ACCOUNT_1_ID);
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(4, foundUsers.getContent().size());
    }

    @Test
    void getEntityByOutId_shouldGetThreeUsers_whenOutIdIsEmpty() {
        var outIdFilter = StringFilter.builder()
                .typeComparison(IS_EMPTY.toString())
                .build();
        var filterDto = UserFilterDtoNew.builder()
                .outId(outIdFilter)
                .build();
        var userSpecification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(User.class, filterDto, ACCOUNT_1_ID);
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(3, foundUsers.getContent().size());
    }

    @Test
    void getEntityByOutId_shouldGetOneUsers_whenOutIdIsExist() {
        var outIdFilter = StringFilter.builder()
                .typeComparison(TEXT_EQUALS.toString())
                .value(USER_3_OUT_ID_EXIST)
                .build();
        var filterDto = UserFilterDtoNew.builder()
                .outId(outIdFilter)
                .build();
        var userSpecification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(User.class, filterDto, ACCOUNT_1_ID);
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(1, foundUsers.getContent().size());
    }


    @Test
    void getEntityByMultipleFilter_shouldGetOneUsers_whenAllConditionalsAreTrueForOneUser() {
        var outIdFilter = StringFilter.builder()
                .typeComparison(TEXT_EQUALS.toString())
                .value(USER_3_OUT_ID_EXIST)
                .build();
        var deletedFilter = BooleanFilter.builder()
                .value(false)
                .build();
        var emailFilter = StringFilter.builder()
                .typeComparison(TEXT_EQUALS.toString())
                .value(USER_3_EMAIL_EXIST)
                .build();
        var filterDto = UserFilterDtoNew.builder()
                .outId(outIdFilter)
                .deleted(deletedFilter)
                .email(emailFilter)
                .build();
        var userSpecification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(User.class, filterDto, ACCOUNT_1_ID);
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertEquals(1, foundUsers.getContent().size());
    }
}