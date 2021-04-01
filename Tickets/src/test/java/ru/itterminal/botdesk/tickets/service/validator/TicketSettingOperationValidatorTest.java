package ru.itterminal.botdesk.tickets.service.validator;

import static java.lang.String.format;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.NOT_UNIQUE_CODE;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.NOT_UNIQUE_MESSAGE;
import static ru.itterminal.botdesk.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createLogicalValidationException;
import static ru.itterminal.botdesk.security.jwt.JwtUserBuilder.USER_FROM_OUTER_GROUP_CANNOT_CREATE_OR_UPDATE_THIS_ENTITY;
import static ru.itterminal.botdesk.tickets.service.validator.TicketSettingOperationValidator.*;

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
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.security.jwt.JwtUserBuilder;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.test.TicketSettingTestHelper;
import ru.itterminal.botdesk.tickets.service.impl.SettingsAccessToTicketTypesServiceImpl;
import ru.itterminal.botdesk.tickets.service.impl.TicketSettingServiceImpl;

@SpringJUnitConfig(value = {TicketSettingOperationValidator.class, JwtUserBuilder.class})
@Import(TestSecurityConfig.class)
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class TicketSettingOperationValidatorTest {

    @MockBean
    private TicketSettingServiceImpl service;

    @MockBean
    private SettingsAccessToTicketTypesServiceImpl settingsAccessToTicketTypesService;

    @SuppressWarnings("unused")
    @Autowired
    private JwtUserBuilder jwtUserBuilder;

    @Autowired
    private TicketSettingOperationValidator validator;

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
        var expectedException =
                createLogicalValidationException(NOT_UNIQUE_CODE, format(NOT_UNIQUE_MESSAGE, TICKET_SETTING_UNIQUE_FIELDS));
        var ticketSetting = ticketSettingTestHelper.getRandomValidEntity();
        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.checkUniqueness(ticketSetting)
                );
        assertEquals(
                expectedException.getFieldErrors().get(NOT_UNIQUE_CODE).get(0),
                actualException.getFieldErrors().get(NOT_UNIQUE_CODE).get(0)
        );
        verify(service, times(1)).findByUniqueFields(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetTrue_whenPassedRandomValidEntity () {
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(true);
        TicketSetting ticketSetting = ticketSettingTestHelper.getRandomValidEntity();
        assertTrue(validator.logicalValidationBeforeCreate(ticketSetting));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetTrue_whenPassedPredefinedValidEntity () {
        TicketSetting ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(true);
        assertTrue(validator.logicalValidationBeforeCreate(ticketSetting));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetTrue_whenPassedRandomValidEntity () {
        when(service.findByUniqueFields(any())).thenReturn(Collections.emptyList());
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(true);
        TicketSetting ticketSetting = ticketSettingTestHelper.getRandomValidEntity();
        assertTrue(validator.logicalValidationBeforeUpdate(ticketSetting));
        verify(service, times(1)).findByUniqueFields(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetTrue_whenPassedPredefinedValidEntity () {
        when(service.findByUniqueFields(any())).thenReturn(Collections.emptyList());
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(true);
        TicketSetting ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        assertTrue(validator.logicalValidationBeforeUpdate(ticketSetting));
        verify(service, times(1)).findByUniqueFields(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenAllPassedSettingIsNull() {
        when(service.findByUniqueFields(any())).thenReturn(Collections.emptyList());
        TicketSetting ticketSetting = new TicketSetting();
        LogicalValidationException exception = assertThrows(LogicalValidationException.class,
                                                            ()-> validator.logicalValidationBeforeUpdate(ticketSetting));
        assertEquals(1, exception.getFieldErrors().get(INVALID_TICKET_SETTINGS).size());
        verify(service, times(1)).findByUniqueFields(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void checkAccessBeforeCreate_shouldGetAccessDeniedException_whenCurrentUserFromNotInnerGroup() {
        var ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        AccessDeniedException thrown = assertThrows(AccessDeniedException.class,
                () -> validator.checkAccessBeforeCreate(ticketSetting));
        assertEquals(USER_FROM_OUTER_GROUP_CANNOT_CREATE_OR_UPDATE_THIS_ENTITY,
                thrown.getMessage());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void checkAccessBeforeUpdate_shouldGetAccessDeniedException_whenCurrentUserFromNotInnerGroup() {
        var ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        AccessDeniedException thrown = assertThrows(AccessDeniedException.class,
                () -> validator.checkAccessBeforeUpdate(ticketSetting));
        assertEquals(USER_FROM_OUTER_GROUP_CANNOT_CREATE_OR_UPDATE_THIS_ENTITY,
                thrown.getMessage());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetTrue_whenCurrentUserFromInnerGroup() {
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(true);
        var ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        assertTrue(validator.logicalValidationBeforeCreate(ticketSetting));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetTrue_whenCurrentUserFromInnerGroup() {
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(true);
        var ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        assertTrue(validator.logicalValidationBeforeUpdate(ticketSetting));
    }

}
