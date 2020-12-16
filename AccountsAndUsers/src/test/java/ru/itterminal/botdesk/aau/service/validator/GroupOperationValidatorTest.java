package ru.itterminal.botdesk.aau.service.validator;

import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.LOGIC_CONSTRAINT_CODE;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.VALIDATION_FAILED;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithAnonymousUser;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.commons.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.projection.GroupUniqueFields;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;

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
    private static LogicalValidationException logicalValidationException;
    private static Group group;
    private static final Map<String, List<ValidationError>> errors = new HashMap<>();
    private static final String USER_FROM_AN_INNER_GROUP_CANNOT_CREATE_UPDATE_GROUPS =
            "A user from not inner group cannot create or update groups";
    private static final String INNER_GROUP = "Inner group";

    @BeforeAll
    static void setUp() {
        Account account1 = new Account();
        account1.setId(UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de"));
        account1.setId(UUID.fromString("bcf98101-2a22-42bf-94cc-c900b50a0b69"));
        group = Group
                .builder()
                .name(EXIST_NAME)
                .account(account1)
                .build();
    }

    @Test
    void checkUniqueness_shouldGetTrue_whenPassedDataIsUnique() {
        when(service.findByUniqueFields(any())).thenReturn(Collections.emptyList());
        assertTrue(validator.checkUniqueness(new Group()));
    }

    @Test
    void checkUniqueness_shouldGetLogicalValidationException_whenPassedDataNotUnique() {
        when(service.findByUniqueFields(any())).thenReturn(List.of(userUniqueFields));
        when(userUniqueFields.getName()).thenReturn(EXIST_NAME);
        errors.put("name", singletonList(new ValidationError("not unique", "name is occupied")));
        logicalValidationException = new LogicalValidationException("Validation failed", errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.checkUniqueness(group));
        assertEquals(logicalValidationException.getFieldErrors().get("name").get(0),
                thrown.getFieldErrors().get("name").get(0));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void checkIsInnerGroupForCreateUpdate_shouldGetLogicalValidationException_whenUserNotInnerGroup() {
        errors.put(INNER_GROUP, singletonList(new ValidationError(LOGIC_CONSTRAINT_CODE,
                USER_FROM_AN_INNER_GROUP_CANNOT_CREATE_UPDATE_GROUPS)));
        logicalValidationException = new LogicalValidationException(VALIDATION_FAILED, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.beforeCreate(group));
        assertEquals(logicalValidationException.getFieldErrors().get(INNER_GROUP).get(0),
                thrown.getFieldErrors().get(INNER_GROUP).get(0));
    }

    @Test
    @WithAnonymousUser
    void checkIsInnerGroupForCreateUpdate_shouldGetLogicalValidationException1_whenUserNotInnerGroup() {
        when(service.create(any())).thenReturn(new Group());
        assertTrue(validator.beforeCreate(new Group()));
    }
}