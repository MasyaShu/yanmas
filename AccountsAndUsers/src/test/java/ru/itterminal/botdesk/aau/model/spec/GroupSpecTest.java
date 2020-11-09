package ru.itterminal.botdesk.aau.model.spec;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

import java.util.UUID;

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

import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.repository.GroupRepository;
import ru.itterminal.botdesk.aau.repository.GroupRepositoryTestConfig;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {GroupRepositoryTestConfig.class, GroupSpec.class})
@Sql({"/create-user-test.sql"})
class GroupSpecTest {

    @Autowired
    private GroupRepository groupRepository;

    @Autowired
    GroupSpec spec;

    private final String GROUP_NAME_1 = "groupName1";
    private final String GROUP_NAME_2 = "groupName2";
    private final String GROUP_NAME_3 = "groupName3";
    private final String GROUP_NAME_NO_FIND = "NoGroup";
    private static final UUID GROUP_1_ID = UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7");
    private static final UUID ACCOUNT_1_ID = UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de");
    private static final UUID ACCOUNT_2_ID = UUID.fromString("bcf98101-2a22-42bf-94cc-c900b50a0b69");

    private final Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "name"));
    private Page<Group> foundGroup;

    @Test
    void getGroupByNameSpec_shouldGetOneGroup_whenNameExistInDatabase() {
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByNameSpec(GROUP_NAME_1.toUpperCase()));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertEquals(foundGroup.getContent().get(0).getName(), GROUP_NAME_1);
        assertEquals(1, foundGroup.getContent().size());
    }

    @Test
    void getGroupByNameSpec_shouldGetThreeGroup_whenSearchByEntryIntoString() {
        String GROUP_NAME_ALL = "Name";
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByNameSpec(GROUP_NAME_ALL.toUpperCase()));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertEquals(foundGroup.getContent().get(0).getName(), GROUP_NAME_1);
        assertEquals(foundGroup.getContent().get(1).getName(), GROUP_NAME_2);
        assertEquals(foundGroup.getContent().get(2).getName(), GROUP_NAME_3);
        assertEquals(3, foundGroup.getContent().size());
    }

    @Test
    void getGroupByNameSpec_shouldGetEmptyList_whenSearchByEntryIntoString() {
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByNameSpec(GROUP_NAME_NO_FIND.toUpperCase()));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertTrue(foundGroup.getContent().isEmpty());
    }

    @Test
    void getGroupByCommentSpec_shouldGetOneGroup_whenNameExistInDatabase() {
        String GROUP_COMMENT_1 = "comment for group1 of users";
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByCommentSpec(GROUP_COMMENT_1.toUpperCase()));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertEquals(foundGroup.getContent().get(0).getName(), GROUP_NAME_1);
        assertEquals(1, foundGroup.getContent().size());
    }

    @Test
    void getGroupByCommentSpec_shouldGetThreeGroup_whenSearchByEntryIntoString() {
        String GROUP_COMMENT_ALL = "comment for group";
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByCommentSpec(GROUP_COMMENT_ALL.toUpperCase()));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertEquals(foundGroup.getContent().get(0).getName(), GROUP_NAME_1);
        assertEquals(foundGroup.getContent().get(1).getName(), GROUP_NAME_2);
        assertEquals(foundGroup.getContent().get(2).getName(), GROUP_NAME_3);
        assertEquals(3, foundGroup.getContent().size());
    }

    @Test
    void getGroupByCommentSpec_shouldGetEmptyList_whenSearchByEntryIntoString() {
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByCommentSpec(GROUP_NAME_NO_FIND.toUpperCase()));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertTrue(foundGroup.getContent().isEmpty());
    }

    @Test
    void getGroupByIsDeprecatedSpec_shouldGetTwoGroup_whenIsDeprecatedFalse() {
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByIsDeprecatedSpec(false));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertEquals(foundGroup.getContent().get(0).getName(), GROUP_NAME_1);
        assertEquals(foundGroup.getContent().get(1).getName(), GROUP_NAME_2);
        assertEquals(2, foundGroup.getContent().size());
    }

    @Test
    void getGroupByIsDeprecatedSpec_shouldGetOneGroup_whenIsDeprecatedTrue() {
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByIsDeprecatedSpec(true));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertEquals(foundGroup.getContent().get(0).getName(), GROUP_NAME_3);
        assertEquals(1, foundGroup.getContent().size());
    }

    @Test
    void getGroupByIsInnerSpec_shouldGetTwoGroup_whenIsDeprecatedFalse() {
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByIsInnerSpec(false));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertEquals(foundGroup.getContent().get(0).getName(), GROUP_NAME_2);
        assertEquals(foundGroup.getContent().get(1).getName(), GROUP_NAME_3);
        assertEquals(2, foundGroup.getContent().size());
    }

    @Test
    void getGroupByIsInnerSpec_shouldGetOneGroup_whenIsDeprecatedTrue() {
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByIsInnerSpec(true));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertEquals(foundGroup.getContent().get(0).getName(), GROUP_NAME_1);
        assertEquals(1, foundGroup.getContent().size());
    }

    @Test
    void getEntityByAccountSpec_shouldGetTwoGroup_whenAccount1() {
        Specification<Group> groupSpecification = Specification
                .where(spec.getEntityByAccountSpec(ACCOUNT_1_ID));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertEquals(foundGroup.getContent().get(0).getName(), GROUP_NAME_1);
        assertEquals(foundGroup.getContent().get(1).getName(), GROUP_NAME_3);
        assertEquals(2, foundGroup.getContent().size());
    }

    @Test
    void getEntityByAccountSpec_shouldGetOneGroup_whenAccount2() {
        Specification<Group> groupSpecification = Specification
                .where(spec.getEntityByAccountSpec(ACCOUNT_2_ID));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertEquals(foundGroup.getContent().get(0).getName(), GROUP_NAME_2);
        assertEquals(1, foundGroup.getContent().size());
    }

    @Test
    void getEntityByAccountSpec_shouldGetThreeGroup_whenAccount1andAccount2() {
        Specification<Group> groupSpecification = Specification
                .where(spec.getEntityByAccountSpec(ACCOUNT_1_ID))
                .or(spec.getEntityByAccountSpec(ACCOUNT_2_ID));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertEquals(foundGroup.getContent().get(0).getName(), GROUP_NAME_1);
        assertEquals(foundGroup.getContent().get(1).getName(), GROUP_NAME_2);
        assertEquals(foundGroup.getContent().get(2).getName(), GROUP_NAME_3);
        assertEquals(3, foundGroup.getContent().size());
    }

    @Test
    void getEntityByAccountSpec_shouldGetEmptyList_whenAccountIdIsNotExistInDatabase() {
        Specification<Group> groupSpecification = Specification
                .where(spec.getEntityByAccountSpec(UUID.randomUUID()));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertTrue(foundGroup.isEmpty());
    }

    @Test
    void getGroupByGroupSpec_shouldGetEmptyList_whenGroupIdIsNotExistInDatabase() {
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByGroupSpec(UUID.randomUUID()));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertTrue(foundGroup.isEmpty());
    }

    @Test
    void getGroupByGroupSpec_shouldGetOneGroup_whenGroupIdIsExistInDatabase() {
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByGroupSpec(GROUP_1_ID));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertEquals(foundGroup.getContent().get(0).getName(), GROUP_NAME_1);
        assertEquals(1, foundGroup.getContent().size());
    }
}