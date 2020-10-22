package ru.itterminal.botdesk.aau.repository;

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
import ru.itterminal.botdesk.aau.model.spec.GroupSpec;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;

import java.util.UUID;

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
    private static final String EXIST_NAME2 = "groupName";
    private static final String NOT_EXIST_NAME = "groupName4";
    private static final UUID EXIST_ID = UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7");
    private static final UUID NOT_EXIST_ID = UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e8");
    private static final UUID ACCOUNT_1_ID = UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de");
    private static final UUID ACCOUNT_2_ID = UUID.fromString("bcf98101-2a22-42bf-94cc-c900b50a0b69");
    private static final String COMMENT_1 = "comment for group1 of users";
    private static final String COMMENT_2 = "comment for";
    private static final String COMMENT_3 = "blablabla";

    @Test
    public void getByNameAndIdNot_shouldGetEmptyList_whenNameNotExistAndIdNotExistInDatabase() {
        assertTrue(groupRepository.getByNameAndIdNot(NOT_EXIST_NAME, NOT_EXIST_ID).isEmpty());
    }

    @Test
    public void getByNameAndIdNot_shouldGetEmptyList_whenNameNotExistAndIdExistInDatabase() {
        assertTrue(groupRepository.getByNameAndIdNot(NOT_EXIST_NAME, EXIST_ID).isEmpty());
    }

    @Test
    public void getByNameAndIdNot_shouldGetNotNull_whenNameExistAndIdNotExistInDatabase() {
        assertTrue(groupRepository.getByNameAndIdNot(EXIST_NAME, NOT_EXIST_ID).get(0).getName().equals(EXIST_NAME));
    }

    @Test
    public void getByNameAndIdNot_shouldGetEmptyList_whenNameExistAndIdExistInDatabase() {
        assertTrue(groupRepository.getByNameAndIdNot(EXIST_NAME, EXIST_ID).isEmpty());
    }

    @Test
    public void getByAll_shouldGetEmptyList_whenDeletedIsTrue() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "name"));
        Page<Group> foundGroup;
        Specification<Group> groupSpecification = Specification
                .where(spec.getEntityByDeletedSpec(BaseFilterDto.FilterByDeleted.fromString("true")));
       foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertTrue(foundGroup.getContent().isEmpty());
    }

    @Test
    public void getByAll_shouldGetThreeGroup_whenDeletedIsFalse() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "name"));
        Page<Group> foundGroup;
        Specification<Group> groupSpecification = Specification
                .where(spec.getEntityByDeletedSpec(BaseFilterDto.FilterByDeleted.fromString("false")));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertTrue(foundGroup.getContent().size() == 3);
    }

    @Test
    public void getByAll_shouldGetTwoGroup_whenAccount1() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "name"));
        Page<Group> foundGroup;
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByAccountSpec(ACCOUNT_1_ID));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertTrue(foundGroup.getContent().size() == 2);
    }

    @Test
    public void getByAll_shouldGetOneGroup_whenAccount2() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "name"));
        Page<Group> foundGroup;
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByAccountSpec(ACCOUNT_2_ID));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertTrue(foundGroup.getContent().size() == 1);
    }

    @Test
    public void getByAll_shouldGetThreeGroup_whenAccount1AndAccount2() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "name"));
        Page<Group> foundGroup;
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByAccountSpec(ACCOUNT_2_ID))
                .or(spec.getGroupByAccountSpec(ACCOUNT_1_ID));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertTrue(foundGroup.getContent().size() == 3);
    }

    @Test
    public void getByAll_shouldGetOneGroup_whenComment1() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "name"));
        Page<Group> foundGroup;
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByCommentSpec(COMMENT_1));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertTrue(foundGroup.getContent().size() == 1);
    }

    @Test
    public void getByAll_shouldGetThreeGroup_whenComment2() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "name"));
        Page<Group> foundGroup;
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByCommentSpec(COMMENT_2));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertTrue(foundGroup.getContent().size() == 3);
    }

    @Test
    public void getByAll_shouldGetEmptyList_whenComment3() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "name"));
        Page<Group> foundGroup;
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByCommentSpec(COMMENT_3));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertTrue(foundGroup.getContent().isEmpty());
    }

    @Test
    public void getByAll_shouldGetOneGroup_whenExistName() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "name"));
        Page<Group> foundGroup;
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByNameSpec(EXIST_NAME));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertTrue(foundGroup.getContent().size() == 1);
    }

    @Test
    public void getByAll_shouldGetThreeGroup_whenExistName2() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "name"));
        Page<Group> foundGroup;
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByNameSpec(EXIST_NAME2));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertTrue(foundGroup.getContent().size() == 3);
    }

    @Test
    public void getByAll_shouldGetEmptyList_whenNotExistName() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "name"));
        Page<Group> foundGroup;
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByCommentSpec(NOT_EXIST_NAME));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertTrue(foundGroup.getContent().isEmpty());
    }

    @Test
    public void getByAll_shouldGetTwoGroup_whenIsInnerFalse() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "name"));
        Page<Group> foundGroup;
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByIsInnerSpec(false));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertTrue(foundGroup.getContent().size() == 2);
    }

    @Test
    public void getByAll_shouldGetOneGroup_whenIsInnerTrue() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "name"));
        Page<Group> foundGroup;
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByIsInnerSpec(true));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertTrue(foundGroup.getContent().size() == 1);
    }

    @Test
    public void getByAll_shouldGetTwoGroup_whenIsDeprecatedFalse() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "name"));
        Page<Group> foundGroup;
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByIsDeprecatedSpec(false));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertTrue(foundGroup.getContent().size() == 2);
    }

    @Test
    public void getByAll_shouldGetOneGroup_whenIsDeprecatedTrue() {
        Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "name"));
        Page<Group> foundGroup;
        Specification<Group> groupSpecification = Specification
                .where(spec.getGroupByIsDeprecatedSpec(true));
        foundGroup = groupRepository.findAll(groupSpecification, pageable);
        assertTrue(foundGroup.getContent().size() == 1);
    }
}