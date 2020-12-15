package ru.itterminal.botdesk.tickets.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.projection.TicketSettingUniqueFields;
import ru.itterminal.botdesk.tickets.model.test.TicketSettingTestHelper;
import ru.itterminal.botdesk.tickets.repository.TicketSettingRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketSettingOperationValidator;

@SpringJUnitConfig(value = {TicketSettingServiceImpl.class})
class TicketSettingServiceImplTest {

    @Autowired
    private TicketSettingServiceImpl service;

    @SuppressWarnings("unused")
    @MockBean
    private TicketSettingOperationValidator validator;

    @MockBean
    private TicketSettingRepository repository;

    private final TicketSettingTestHelper ticketSettingTestHelper = new TicketSettingTestHelper();

    @Test
    void findByUniqueFields_shouldGetEmptyList_whenPassedDataIsUnique() {
        when(repository.findByUniqueFields(any(), any(), any(), any())).thenReturn(Collections.emptyList());
        TicketSetting ticketSetting = ticketSettingTestHelper.getRandomValidEntity();
        List<TicketSettingUniqueFields> list = service.findByUniqueFields(ticketSetting);
        assertTrue(list.isEmpty());
        verify(repository, times(1)).findByUniqueFields(any(), any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenTicketSettingIsNull() {
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(null));
        verify(repository, times(0)).findByUniqueFields(any(), any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetLogicalValidationException_whenAccountGroupAuthorAreNull() {
        TicketSetting ticketSetting = ticketSettingTestHelper.getRandomValidEntity();
        ticketSetting.setAccount(null);
        ticketSetting.setGroup(null);
        ticketSetting.setAuthor(null);
        LogicalValidationException exception = assertThrows(
                LogicalValidationException.class,
                () -> service.findByUniqueFields(ticketSetting)
        );
        assertEquals(3, exception.getFieldErrors().size());
        verify(repository, times(0)).findByUniqueFields(any(), any(), any(), any());
    }

}