package ru.itterminal.botdesk.aau.repository;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.dao.InvalidDataAccessApiUsageException;
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
import ru.itterminal.botdesk.aau.model.spec.UserSpec;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {UserRepositoryTestConfig.class, UserSpec.class, RoleRepository.class})
@Sql({"/create-user-test.sql"})
class UserRepositoryTest {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private RoleRepository roleRepository;

    @Autowired
    UserSpec spec;

    private static final String EXIST_EMAIL = "m@m.ru";
    private static final String NOT_EXIST_EMAIL = "n@n.ru";
    private static final UUID EXIST_ID = UUID.fromString("d592facb-e6ee-4801-8310-9c7708eb6e6c");
    private static final UUID NOT_EXIST_ID = UUID.fromString("cb9e8816-7bed-4bb6-b3ea-3aa0eee247b6");
    private static final UUID ACCOUNT_1_ID = UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de");
    private static final UUID ACCOUNT_2_ID = UUID.fromString("bcf98101-2a22-42bf-94cc-c900b50a0b69");
    private static final UUID GROUP_1_ID = UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7");
    private static final UUID GROUP_2_ID = UUID.fromString("99f6b488-7687-4451-b8a1-9fbeb2a3efec");
    private static final UUID ROLE_ACCOUNT_OWNER_ID = UUID.fromString("ba99ce38-1611-4a81-adc9-3a779d58bbfe");
    private static final UUID ROLE_ADMIN_ID = UUID.fromString("607f04b1-f5f9-4f20-9c6f-501c32d773c0");
    private static final UUID ROLE_AUTHOR_ID = UUID.fromString("933f20bf-9262-47bb-83d2-0ca55bbbd3fd");

    @Test
    public void getByEmailAndIdNot_shouldGetEmptyList_whenEmailNotExistAndIdNotExistInDatabase() {
        assertTrue(userRepository.getByEmailAndIdNot(NOT_EXIST_EMAIL, NOT_EXIST_ID).isEmpty());
    }

    @Test
    public void getByEmailAndIdNot_shouldGetEmptyList_whenEmailNotExistAndIdExistInDatabase() {
        assertTrue(userRepository.getByEmailAndIdNot(NOT_EXIST_EMAIL, EXIST_ID).isEmpty());
    }

    @Test
    public void getByEmailAndIdNot_shouldGetNotNull_whenEmailExistAndIdNotExistInDatabase() {
        assertTrue(userRepository.getByEmailAndIdNot(EXIST_EMAIL, NOT_EXIST_ID).get(0).getEmail().equals(EXIST_EMAIL));
    }

    @Test
    public void getByEmailAndIdNot_shouldGetEmptyList_whenEmailExistAndIdExistInDatabase() {
        assertTrue(userRepository.getByEmailAndIdNot(EXIST_EMAIL, EXIST_ID).isEmpty());
    }

    // TODO add test with getUserByAccountSpec

    @Test
    public void getByAll_shouldGetEmptyList_whenDeletedIsTrue() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "firstName"));
        Page<User> foundUsers;
        Specification<User> userSpecification = Specification
                .where(spec.getEntityByDeletedSpec(BaseFilterDto.FilterByDeleted.fromString("true")));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().isEmpty());
    }

    @Test
    public void getByAll_shouldGetFiveUsers_whenDeletedIsFalse() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "firstName"));
        Page<User> foundUsers;
        Specification<User> userSpecification = Specification
                .where(spec.getEntityByDeletedSpec(BaseFilterDto.FilterByDeleted.fromString("false")));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().size() == 5);
    }

    @Test
    public void getByAll_shouldGetFourUsers_whenAccount1() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "firstName"));
        Page<User> foundUsers;
        Specification<User> userSpecification = Specification
                .where(spec.getUserByAccountSpec(ACCOUNT_1_ID));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().size() == 4);
    }

    @Test
    public void getByAll_shouldGetOneUser_whenAccount2() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "firstName"));
        Page<User> foundUsers;
        Specification<User> userSpecification = Specification
                .where(spec.getUserByAccountSpec(ACCOUNT_2_ID));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().size() == 1);
    }

    @Test
    public void getByAll_shouldGetFiveUsers_whenAccount1AndAccount2() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "firstName"));
        Page<User> foundUsers;
        Specification<User> userSpecification = Specification
                .where(spec.getUserByAccountSpec(ACCOUNT_2_ID))
                .or(spec.getUserByAccountSpec(ACCOUNT_1_ID));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().size() == 5);
    }

    @Test
    public void getByAll_shouldGetFourUsers_whenGroup1() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "firstName"));
        Page<User> foundUsers;
        Specification<User> userSpecification = Specification
                .where(spec.getUserByListOfGroupsSpec(List.of(GROUP_1_ID)));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().size() == 4);
    }

    @Test
    public void getByAll_shouldGetOneUser_whenGroup2() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "firstName"));
        Page<User> foundUsers;
        Specification<User> userSpecification = Specification
                .where(spec.getUserByListOfGroupsSpec(List.of(GROUP_2_ID)));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().size() == 1);
    }

    @Test
    public void getByAll_shouldGetFiveUsers_whenArrayOfGroups() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "firstName"));
        Page<User> foundUsers;
        Specification<User> userSpecification = Specification
                .where(spec.getUserByListOfGroupsSpec(List.of(GROUP_1_ID, GROUP_2_ID)));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().size() == 5);
    }

    @Test
    public void getByAll_shouldGetFiveUsers_whenArrayOfRoles() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "firstName"));
        Page<User> foundUsers;
        Specification<User> userSpecification = Specification
                .where(spec.getUserByListOfRolesSpec(List.of(ROLE_ACCOUNT_OWNER_ID, ROLE_AUTHOR_ID)));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().size() == 5);
    }

    @Test
    public void getByAll_shouldGetOneUser_whenRoleIsAccountOwner() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "firstName"));
        Page<User> foundUsers;
        Specification<User> userSpecification = Specification
                .where(spec.getUserByListOfRolesSpec(List.of(ROLE_ACCOUNT_OWNER_ID)));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().size() == 1);
    }

    @Test
    public void getByAll_shouldGetEmpty_whenRoleIsAdmin() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "firstName"));
        Page<User> foundUsers;
        Specification<User> userSpecification = Specification
                .where(spec.getUserByListOfRolesSpec(List.of(ROLE_ADMIN_ID)));
        foundUsers = userRepository.findAll(userSpecification, pageable);
        assertTrue(foundUsers.getContent().isEmpty());
    }

    @Test
    public void getByRolesAndIdNot_shouldGetEmpty_whenAccountOwnerExistAndIdEquals() {
        List<User> foundUsers;
        Role role = roleRepository.getByName(Roles.ACCOUNT_OWNER.toString()).get();
        foundUsers = userRepository.findAllByRolesAndIdNot(role, EXIST_ID);
        assertTrue(foundUsers.isEmpty());
    }

    @Test
    public void getByRolesAndIdNot_shouldGetOneUser_whenAccountOwnerExistAndIdNotEquals() {
        List<User> foundUsers;
        Role role = roleRepository.getByName(Roles.ACCOUNT_OWNER.toString()).get();
        foundUsers = userRepository.findAllByRolesAndIdNot(role, UUID.randomUUID());
        assertTrue(foundUsers.size() == 1);
    }

    @Test
    public void getByRolesAndIdNot_shouldGetInvalidDataAccessApiUsageException_whenAccountOwnerExistAndIdIsNull() {
        List<User> foundUsers;
        Role role = roleRepository.getByName(Roles.ACCOUNT_OWNER.toString()).get();
        assertThrows(InvalidDataAccessApiUsageException.class, () -> userRepository.findAllByRolesAndIdNot(role, null));
    }

}