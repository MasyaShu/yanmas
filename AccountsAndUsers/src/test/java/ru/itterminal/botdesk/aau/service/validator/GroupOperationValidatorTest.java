package ru.itterminal.botdesk.aau.service.validator;

import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithAnonymousUser;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.projection.GroupUniqueFields;
import ru.itterminal.botdesk.aau.model.test.GroupTestHelper;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.lang.String.format;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.*;

@SpringJUnitConfig(value = {GroupOperationValidator.class})
@Import(TestSecurityConfig.class)
@ActiveProfiles("Test")
class GroupOperationValidatorTest {

    @MockBean
    private GroupServiceImpl service;

    @Mock
    private GroupUniqueFields userUniqueFields;

    @Autowired
    private final GroupOperationValidator validator = new GroupOperationValidator(service);

    private static final String EXIST_NAME = "groupName1";
    private static final String VALIDATED_FIELDS = "name";
    private static LogicalValidationException logicalValidationException;
    private static final Map<String, List<ValidationError>> errors = new HashMap<>();
    private static final String USER_FROM_AN_INNER_GROUP_CANNOT_CREATE_UPDATE_GROUPS =
            "A user from not inner group cannot create or update groups";
    private static final String INNER_GROUP = "Inner group";
    private final GroupTestHelper groupTestHelper = new GroupTestHelper();

    @Test
    void checkUniqueness_shouldGetTrue_whenPassedDataUnique() {
        Group group = groupTestHelper.getRandomValidEntity();
        when(service.findByUniqueFields(any())).thenReturn(Collections.emptyList());
        assertTrue(validator.checkUniqueness(group));
        verify(service, times(1)).findByUniqueFields(any());
    }

    @Test
    void checkUniqueness_shouldGetLogicalValidationException_whenPassedDataNotUnique() {
        Group group = groupTestHelper.getRandomValidEntity();
        group.setName(EXIST_NAME);
        when(service.findByUniqueFields(any())).thenReturn(List.of(userUniqueFields));
        when(userUniqueFields.getName()).thenReturn(EXIST_NAME);
        errors.put("name", singletonList(new ValidationError(VALIDATED_FIELDS, "name is occupied")));
        logicalValidationException = new LogicalValidationException(VALIDATED_FIELDS, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.checkUniqueness(group));
        assertEquals(logicalValidationException.getFieldErrors().get("name").get(0),
                thrown.getFieldErrors().get("name").get(0));
        verify(service, times(1)).findByUniqueFields(any());
    }

    @Test
    @WithAnonymousUser
    void checkIsInnerGroupForCreateUpdate_shouldGetNoError_whenAnonymousUser() {
        Group group = groupTestHelper.getRandomValidEntity();
        when(service.create(any())).thenReturn(group);
        assertTrue(validator.beforeCreate(group));
    }

    @Test
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void checkIsInnerGroupForCreateUpdate_shouldGetLogicalValidationException_whenUserNotInnerGroup() {
        Group group = groupTestHelper.getRandomValidEntity();
        errors.put(INNER_GROUP, singletonList(new ValidationError(LOGIC_CONSTRAINT_CODE,
                USER_FROM_AN_INNER_GROUP_CANNOT_CREATE_UPDATE_GROUPS)));
        logicalValidationException = new LogicalValidationException(VALIDATION_FAILED, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.beforeCreate(group));
        assertEquals(logicalValidationException.getFieldErrors().get(INNER_GROUP).get(0),
                thrown.getFieldErrors().get(INNER_GROUP).get(0));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void checkIsInnerGroupForCreateUpdate_shouldGetNoErrors_whenUserInnerGroup() {
        Group group = groupTestHelper.getRandomValidEntity();
        when(service.create(any())).thenReturn(group);
        assertTrue(validator.beforeCreate(group));
    }
}