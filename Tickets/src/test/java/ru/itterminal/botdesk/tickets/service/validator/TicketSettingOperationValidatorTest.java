package ru.itterminal.botdesk.tickets.service.validator;

import static java.lang.String.format;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.NOT_UNIQUE_CODE;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.NOT_UNIQUE_MESSAGE;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.VALIDATION_FAILED;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.botdesk.tickets.service.validator.TicketSettingOperationValidator.A_USER_FROM_NOT_INNER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_SETTING;
import static ru.itterminal.botdesk.tickets.service.validator.TicketSettingOperationValidator.GROUPS_ARENT_EQUAL;
import static ru.itterminal.botdesk.tickets.service.validator.TicketSettingOperationValidator.GROUPS_ARENT_EQUAL_MESSAGE;
import static ru.itterminal.botdesk.tickets.service.validator.TicketSettingOperationValidator.TICKET_SETTING_IS_EMPTY;
import static ru.itterminal.botdesk.tickets.service.validator.TicketSettingOperationValidator.TICKET_SETTING_UNIQUE_FIELDS;

import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.test.TicketSettingTestHelper;
import ru.itterminal.botdesk.tickets.service.impl.TicketSettingServiceImpl;

@SpringJUnitConfig(value = {TicketSettingOperationValidator.class})
@Import(TestSecurityConfig.class)
@ActiveProfiles("Test")
class TicketSettingOperationValidatorTest {

    public static final String WRONG_NAME = "Wrong name";
    @MockBean
    private TicketSettingServiceImpl service;

    @Autowired
    private final TicketSettingOperationValidator validator = new TicketSettingOperationValidator(service);

    private final TicketSettingTestHelper ticketSettingTestHelper = new TicketSettingTestHelper();

    @Test
    void checkUniqueness_shouldGetTrue_whenTicketSettingIsUnique() {
        when(service.findByUniqueFields(any())).thenReturn(Collections.emptyList());
        assertTrue(validator.checkUniqueness(new TicketSetting()));
        verify(service, times(1)).findByUniqueFields(any());

    }

    @Test
    void checkUniqueness_shouldGetLogicalValidationException_whenPassedDataNotUnique() {
        when(service.findByUniqueFields(any())).thenReturn(List.of(new TicketSetting()));
        var expectedErrors = createMapForLogicalErrors();
        expectedErrors.put(
                TICKET_SETTING_UNIQUE_FIELDS,
                singletonList(
                        new ValidationError(
                                NOT_UNIQUE_CODE,
                                format(NOT_UNIQUE_MESSAGE, TICKET_SETTING_UNIQUE_FIELDS)
                        )
                )
        );
        LogicalValidationException expectedException =
                new LogicalValidationException(VALIDATION_FAILED, expectedErrors);
        TicketSetting ticketSetting = ticketSettingTestHelper.getRandomValidEntity();
        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.checkUniqueness(ticketSetting)
                );
        assertEquals(
                expectedException.getFieldErrors().get(TICKET_SETTING_UNIQUE_FIELDS).get(0),
                actualException.getFieldErrors().get(TICKET_SETTING_UNIQUE_FIELDS).get(0)
        );
        verify(service, times(1)).findByUniqueFields(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetTrue_whenPassedRandomValidEntity () {
        TicketSetting ticketSetting = ticketSettingTestHelper.getRandomValidEntity();
        assertTrue(validator.beforeCreate(ticketSetting));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetTrue_whenPassedPredefinedValidEntity () {
        TicketSetting ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        assertTrue(validator.beforeCreate(ticketSetting));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetTrue_whenPassedRandomValidEntity () {
        when(service.findByUniqueFields(any())).thenReturn(Collections.emptyList());
        TicketSetting ticketSetting = ticketSettingTestHelper.getRandomValidEntity();
        assertTrue(validator.beforeUpdate(ticketSetting));
        verify(service, times(1)).findByUniqueFields(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetTrue_whenPassedPredefinedValidEntity () {
        when(service.findByUniqueFields(any())).thenReturn(Collections.emptyList());
        TicketSetting ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        assertTrue(validator.beforeUpdate(ticketSetting));
        verify(service, times(1)).findByUniqueFields(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenAllPassedSettingIsNull() {
        when(service.findByUniqueFields(any())).thenReturn(Collections.emptyList());
        TicketSetting ticketSetting = new TicketSetting();
        LogicalValidationException exception = assertThrows(LogicalValidationException.class,
                                                            ()-> validator.beforeUpdate(ticketSetting));
        assertEquals(1, exception.getFieldErrors().get(TICKET_SETTING_IS_EMPTY).size());
        verify(service, times(1)).findByUniqueFields(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetLogicalValidationException_whenPassedTicketSettingWithDifferentGroupAndGroupOfAuthor() {
        var expectedErrors = createMapForLogicalErrors();
        var ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        var groupOfAuthor = ticketSetting.getAuthor().getGroup();
        var groupOfEntity = ticketSetting.getGroup();
        groupOfAuthor.setName(WRONG_NAME);
        ticketSetting.getAuthor().setGroup(groupOfAuthor);
        expectedErrors.put(
                GROUPS_ARENT_EQUAL,
                singletonList(
                        new ValidationError(
                                GROUPS_ARENT_EQUAL,
                                format(GROUPS_ARENT_EQUAL_MESSAGE, groupOfEntity, groupOfAuthor)
                        )
                )
        );
        LogicalValidationException expectedException =
                new LogicalValidationException(VALIDATION_FAILED, expectedErrors);

        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.beforeCreate(ticketSetting)
                );
        assertEquals(
                expectedException.getFieldErrors().get(GROUPS_ARENT_EQUAL).get(0),
                actualException.getFieldErrors().get(GROUPS_ARENT_EQUAL).get(0)
        );
        verify(service, times(0)).findByUniqueFields(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenPassedTicketSettingWithDifferentGroupAndGroupOfAuthor() {
        var expectedErrors = createMapForLogicalErrors();
        var ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        var groupOfAuthor = ticketSetting.getAuthor().getGroup();
        var groupOfEntity = ticketSetting.getGroup();
        groupOfAuthor.setName(WRONG_NAME);
        ticketSetting.getAuthor().setGroup(groupOfAuthor);
        expectedErrors.put(
                GROUPS_ARENT_EQUAL,
                singletonList(
                        new ValidationError(
                                GROUPS_ARENT_EQUAL,
                                format(GROUPS_ARENT_EQUAL_MESSAGE, groupOfEntity, groupOfAuthor)
                        )
                )
        );
        LogicalValidationException expectedException =
                new LogicalValidationException(VALIDATION_FAILED, expectedErrors);

        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.beforeUpdate(ticketSetting)
                );
        assertEquals(
                expectedException.getFieldErrors().get(GROUPS_ARENT_EQUAL).get(0),
                actualException.getFieldErrors().get(GROUPS_ARENT_EQUAL).get(0)
        );
        verify(service, times(1)).findByUniqueFields(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeCreate_shouldGetAccessDeniedException_whenCurrentUserFromNotInnerGroup() {
        var ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        AccessDeniedException thrown = assertThrows(AccessDeniedException.class,
                () -> validator.beforeCreate(ticketSetting));
        assertEquals(A_USER_FROM_NOT_INNER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_SETTING,
                thrown.getMessage());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeUpdate_shouldGetAccessDeniedException_whenCurrentUserFromNotInnerGroup() {
        var ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        AccessDeniedException thrown = assertThrows(AccessDeniedException.class,
                () -> validator.beforeUpdate(ticketSetting));
        assertEquals(A_USER_FROM_NOT_INNER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_SETTING,
                thrown.getMessage());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetTrue_whenCurrentUserFromInnerGroup() {
        var ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        assertTrue(validator.beforeCreate(ticketSetting));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetTrue_whenCurrentUserFromInnerGroup() {
        var ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        assertTrue(validator.beforeUpdate(ticketSetting));
    }

}