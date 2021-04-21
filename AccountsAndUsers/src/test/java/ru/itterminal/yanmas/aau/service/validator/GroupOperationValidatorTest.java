package ru.itterminal.yanmas.aau.service.validator;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.test.context.support.WithAnonymousUser;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.projection.GroupUniqueFields;
import ru.itterminal.yanmas.aau.model.test.GroupTestHelper;
import ru.itterminal.yanmas.aau.repository.GroupRepository;
import ru.itterminal.yanmas.aau.service.impl.GroupServiceImpl;
import ru.itterminal.yanmas.commons.exception.LogicalValidationException;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.security.config.TestSecurityConfig;

import java.util.*;

import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl.LOGIC_CONSTRAINT_CODE;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;
import static ru.itterminal.yanmas.security.config.TestSecurityConfig.NOT_INNER_GROUP_ID;

@SpringJUnitConfig(value = {GroupOperationValidator.class})
@Import(TestSecurityConfig.class)
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class GroupOperationValidatorTest {

    @MockBean
    private GroupRepository repository;

    @MockBean
    private GroupServiceImpl service;

    @MockBean
    private GroupUniqueFields userUniqueFields;

    @Autowired
    private final GroupOperationValidator validator = new GroupOperationValidator(service, repository);

    private static final String EXIST_NAME = "groupName1";
    private static final String VALIDATED_FIELDS = "name";
    private static final Map<String, List<ValidationError>> errors = new HashMap<>();
    private static final String USER_FROM_AN_INNER_GROUP_CANNOT_CREATE_UPDATE_GROUPS =
            "A user from not inner group cannot create or update groups";
    private static final String INNER_GROUP = "Inner group";
    private final GroupTestHelper groupTestHelper = new GroupTestHelper();

    @Test
    void checkUniqueness_shouldGetTrue_whenPassedDataUnique() {
        Group group = groupTestHelper.getRandomValidEntity();
        when(repository.getByNameAndIsInnerAndAccount_IdAndIdNot(any(), any(), any(), any())).thenReturn(Collections.emptyList());
        assertTrue(validator.checkUniqueness(group));
        verify(repository, times(1)).getByNameAndIsInnerAndAccount_IdAndIdNot(any(), any(), any(), any());
    }

    @Test
    void checkUniqueness_shouldGetLogicalValidationException_whenPassedDataNotUnique() {
        Group group = groupTestHelper.getRandomValidEntity();
        group.setName(EXIST_NAME);
        when(repository.getByNameAndIsInnerAndAccount_IdAndIdNot(any(), any(), any(), any())).thenReturn(List.of(userUniqueFields));
        when(userUniqueFields.getName()).thenReturn(EXIST_NAME);
        errors.put("name", singletonList(new ValidationError(VALIDATED_FIELDS, "name is occupied")));
        LogicalValidationException logicalValidationException = new LogicalValidationException(VALIDATED_FIELDS, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.checkUniqueness(group));
        assertEquals(logicalValidationException.getFieldErrors().get("name").get(0),
                thrown.getFieldErrors().get("name").get(0));
        verify(repository, times(1)).getByNameAndIsInnerAndAccount_IdAndIdNot(any(), any(), any(), any());
    }

    @Test
    @WithAnonymousUser
    void checkIsInnerGroupForCreateUpdate_shouldGetNoError_whenAnonymousUser() {
        Group group = groupTestHelper.getRandomValidEntity();
        when(service.create(any())).thenReturn(group);
        assertTrue(validator.logicalValidationBeforeCreate(group));
    }

    @Test
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void checkIsInnerGroupForCreateUpdate_shouldGetLogicalValidationException_whenUserExecutorNotInnerGroup() {
        Group group = groupTestHelper.getRandomValidEntity();
        errors.put(INNER_GROUP, singletonList(new ValidationError(LOGIC_CONSTRAINT_CODE,
                USER_FROM_AN_INNER_GROUP_CANNOT_CREATE_UPDATE_GROUPS)));
        AccessDeniedException thrown = assertThrows(AccessDeniedException.class,
                () -> validator.checkAccessBeforeCreate(group));
        assertEquals(USER_FROM_AN_INNER_GROUP_CANNOT_CREATE_UPDATE_GROUPS,
                thrown.getMessage());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void checkIsInnerGroupForCreateUpdate_shouldGetNoErrors_whenUserInnerGroup() {
        Group group = groupTestHelper.getRandomValidEntity();
        when(service.create(any())).thenReturn(group);
        assertTrue(validator.logicalValidationBeforeCreate(group));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void checkAccessForRead_shouldNotGetException_whenUserInnerGroup() {
        var group = groupTestHelper.getRandomValidEntity();
        validator.checkAccessBeforeRead(group);
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void checkAccessForRead_shouldGetAccessError_whenUserNotInnerGroupAndGroupUserNoEqualsEntity() {
        Group group = groupTestHelper.getRandomValidEntity();
        group.setId(UUID.randomUUID());
        assertThrows(AccessDeniedException.class,
                () -> validator.checkAccessBeforeRead(group));
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_2_IS_INNER_GROUP")
    void checkAccessForRead_shouldGetAccessError_whenUserByInnerGroupRoleAuthor() {
        Group group = groupTestHelper.getRandomValidEntity();
        group.setId(UUID.randomUUID());
        assertThrows(AccessDeniedException.class,
                () -> validator.checkAccessBeforeRead(group));
    }

    @Test
    @WithUserDetails("OBSERVER_ACCOUNT_1_IS_INNER_GROUP")
    void checkAccessForRead_shouldGetAccessError_whenUserByInnerGroupRoleObserver() {
        Group group = groupTestHelper.getRandomValidEntity();
        group.setId(UUID.randomUUID());
        assertThrows(AccessDeniedException.class,
                () -> validator.checkAccessBeforeRead(group));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void checkAccessForRead_shouldNotGetException_whenUserNotInnerGroupAndGroupUserEqualsEntity() {
        var group = groupTestHelper.getRandomValidEntity();
        group.setId(UUID.fromString(NOT_INNER_GROUP_ID));
        validator.checkAccessBeforeRead(group);
    }
}
