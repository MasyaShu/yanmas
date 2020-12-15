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
import static ru.itterminal.botdesk.tickets.service.validator.TicketSettingOperationValidator.ACCOUNTS_ARENT_EQUAL;
import static ru.itterminal.botdesk.tickets.service.validator.TicketSettingOperationValidator.GROUPS_ARENT_EQUAL;
import static ru.itterminal.botdesk.tickets.service.validator.TicketSettingOperationValidator.TICKET_SETTING_UNIQUE_FIELDS;

import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.projection.TicketSettingUniqueFields;
import ru.itterminal.botdesk.tickets.model.test.TicketSettingTestHelper;
import ru.itterminal.botdesk.tickets.service.impl.TicketSettingServiceImpl;

@SpringJUnitConfig(value = {TicketSettingOperationValidator.class})
class TicketSettingOperationValidatorTest {

    @MockBean
    private TicketSettingServiceImpl service;

    @Mock
    private TicketSettingUniqueFields ticketSettingUniqueFields;

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
        when(service.findByUniqueFields(any())).thenReturn(List.of(ticketSettingUniqueFields));
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
    void beforeCreate_shouldGetTrue_whenPassedRandomValidEntity () {
        TicketSetting ticketSetting = ticketSettingTestHelper.getRandomValidEntity();
        assertTrue(validator.beforeCreate(ticketSetting));
    }

    @Test
    void beforeCreate_shouldGetTrue_whenPassedPredefinedValidEntity () {
        TicketSetting ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        assertTrue(validator.beforeCreate(ticketSetting));
    }

    @Test
    void beforeUpdate_shouldGetTrue_whenPassedRandomValidEntity () {
        when(service.findByUniqueFields(any())).thenReturn(Collections.emptyList());
        TicketSetting ticketSetting = ticketSettingTestHelper.getRandomValidEntity();
        assertTrue(validator.beforeUpdate(ticketSetting));
        verify(service, times(1)).findByUniqueFields(any());
    }

    @Test
    void beforeUpdate_shouldGetTrue_whenPassedPredefinedValidEntity () {
        when(service.findByUniqueFields(any())).thenReturn(Collections.emptyList());
        TicketSetting ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        assertTrue(validator.beforeUpdate(ticketSetting));
        verify(service, times(1)).findByUniqueFields(any());
    }

    @Test
    void beforeUpdate_shouldGetLogicalValidationException_whenPassedRandomInvalidEntity() {
        when(service.findByUniqueFields(any())).thenReturn(Collections.emptyList());
        TicketSetting ticketSetting = ticketSettingTestHelper.getRandomInvalidEntity();
        LogicalValidationException exception = assertThrows(LogicalValidationException.class,
                     ()-> validator.beforeUpdate(ticketSetting));
        assertEquals(2, exception.getFieldErrors().size());
        assertEquals(17, exception.getFieldErrors().get(ACCOUNTS_ARENT_EQUAL).size());
        assertEquals(1, exception.getFieldErrors().get(GROUPS_ARENT_EQUAL).size());
        verify(service, times(1)).findByUniqueFields(any());
    }

}