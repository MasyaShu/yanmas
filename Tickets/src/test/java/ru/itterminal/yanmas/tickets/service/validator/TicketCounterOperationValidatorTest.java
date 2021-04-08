package ru.itterminal.yanmas.tickets.service.validator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.yanmas.commons.exception.LogicalValidationException;
import ru.itterminal.yanmas.security.jwt.JwtUser;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;
import ru.itterminal.yanmas.tickets.model.TicketCounter;
import ru.itterminal.yanmas.tickets.service.impl.TicketCounterServiceImpl;

@SpringJUnitConfig(value = {TicketCounterOperationValidator.class})
class TicketCounterOperationValidatorTest {

    @Autowired
    private TicketCounterOperationValidator validator;

    @MockBean
    private TicketCounterServiceImpl service;

    @SuppressWarnings("unused")
    @MockBean
    private JwtUserBuilder jwtUserBuilder;

    @Test
    void beforeUpdate_shouldGetTrue_whenCurrentNumberFromDatabaseLessThanPassedValue() {
        var returnedJwtUser = JwtUser.builder()
                .isInnerGroup(true)
                .build();
        when(jwtUserBuilder.getJwtUser()).thenReturn(returnedJwtUser);
        var ticketCounterFromDatabase = TicketCounter.builder()
                .currentNumber(10L)
                .build();
        when(service.findById(any())).thenReturn(ticketCounterFromDatabase);
        var newCounterFromDatabase = TicketCounter.builder()
                .currentNumber(15L)
                .build();
        assertTrue(validator.logicalValidationBeforeUpdate(newCounterFromDatabase));
        verify(service, times(1)).findById(any());
    }

    @Test
    void beforeUpdate_shouldGetLogicalValidationException_whenCurrentNumberFromDatabaseMoreThanPassedValue() {
        var returnedJwtUser = JwtUser.builder()
                .isInnerGroup(true)
                .build();
        when(jwtUserBuilder.getJwtUser()).thenReturn(returnedJwtUser);
        var ticketCounterFromDatabase = TicketCounter.builder()
                .currentNumber(10L)
                .build();
        when(service.findById(any())).thenReturn(ticketCounterFromDatabase);
        var newCounterFromDatabase = TicketCounter.builder()
                .currentNumber(5L)
                .build();
        var actualException =
                assertThrows(LogicalValidationException.class, () -> validator.logicalValidationBeforeUpdate(newCounterFromDatabase));
        Assertions.assertEquals(TicketCounterOperationValidator.NEW_VALUE_MUST_NOT, actualException.getFieldErrors().get(TicketCounterOperationValidator.CURRENT_TICKET_NUMBER).get(0).getMessage());
        verify(service, times(1)).findById(any());
    }

    @Test
    void beforeUpdate_shouldGetAccessDeniedException_whenUserFromOuterGroup() {
        var returnedJwtUser = JwtUser.builder()
                .isInnerGroup(false)
                .build();
        when(jwtUserBuilder.getJwtUser()).thenReturn(returnedJwtUser);
        var ticketCounterForUpdate = TicketCounter.builder()
                .currentNumber(100L)
                .build();
        var actualException =
                assertThrows(AccessDeniedException.class, () -> validator.checkAccessBeforeUpdate(ticketCounterForUpdate));
        Assertions.assertEquals(TicketCounterOperationValidator.USER_FROM_OUTER_GROUP_CANT_UPDATE_TICKET_COUNTER, actualException.getMessage());
        verify(service, times(0)).findById(any());
        verify(service, times(0)).update(any());
    }


}