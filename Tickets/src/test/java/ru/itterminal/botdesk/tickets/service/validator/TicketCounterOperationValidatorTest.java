package ru.itterminal.botdesk.tickets.service.validator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static ru.itterminal.botdesk.tickets.service.validator.TicketCounterOperationValidator.CURRENT_TICKET_NUMBER;
import static ru.itterminal.botdesk.tickets.service.validator.TicketCounterOperationValidator.NEW_VALUE_MUST_NOT;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.tickets.model.TicketCounter;
import ru.itterminal.botdesk.tickets.service.impl.TicketCounterServiceImpl;

@SpringJUnitConfig(value = {TicketCounterOperationValidator.class})
class TicketCounterOperationValidatorTest {

    @Autowired
    TicketCounterOperationValidator validator;

    @MockBean
    TicketCounterServiceImpl service;

    @Test
    void beforeUpdate_shouldGetTrue_whenCurrentNumberFromDatabaseLessThanPassedValue() {
        var ticketCounterFromDatabase = TicketCounter.builder()
                .currentNumber(10L)
                .build();
        when(service.findById(any())).thenReturn(ticketCounterFromDatabase);
        var newCounterFromDatabase = TicketCounter.builder()
                .currentNumber(15L)
                .build();
        assertTrue(validator.beforeUpdate(newCounterFromDatabase));
        verify(service, times(1)).findById(any());
    }

    @Test
    void beforeUpdate_shouldGetLogicalValidationException_whenCurrentNumberFromDatabaseMoreThanPassedValue() {
        var ticketCounterFromDatabase = TicketCounter.builder()
                .currentNumber(10L)
                .build();
        when(service.findById(any())).thenReturn(ticketCounterFromDatabase);
        var newCounterFromDatabase = TicketCounter.builder()
                .currentNumber(5L)
                .build();
        var actualException =
                assertThrows(LogicalValidationException.class, () -> validator.beforeUpdate(newCounterFromDatabase));
        assertEquals(NEW_VALUE_MUST_NOT, actualException.getFieldErrors().get(CURRENT_TICKET_NUMBER).get(0).getMessage());
        verify(service, times(1)).findById(any());
    }

}