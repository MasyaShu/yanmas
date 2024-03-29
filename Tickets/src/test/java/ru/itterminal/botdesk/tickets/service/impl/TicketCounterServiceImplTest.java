package ru.itterminal.botdesk.tickets.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.tickets.model.TicketCounter;
import ru.itterminal.botdesk.tickets.repository.TicketCounterRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketCounterOperationValidator;

@SpringJUnitConfig(value = {TicketCounterServiceImpl.class})
@TestInstance(PER_CLASS)
class TicketCounterServiceImplTest {

    @Autowired
    private TicketCounterServiceImpl service;

    @MockBean
    private TicketCounterOperationValidator validator;

    @MockBean
    private TicketCounterRepository repository;

    private TicketCounter ticketCounterWithNumber1;
    private TicketCounter ticketCounterWithNumber2;
    private TicketCounter ticketCounterWithNumber3;

    @BeforeAll
    void setupBeforeAll() {
        ticketCounterWithNumber1 = TicketCounter.builder()
                .currentNumber(1L)
                .id(UUID.randomUUID())
                .deleted(false)
                .version(0)
                .build();
        ticketCounterWithNumber2 = TicketCounter.builder()
                .currentNumber(2L)
                .id(UUID.randomUUID())
                .deleted(false)
                .version(1)
                .build();
        ticketCounterWithNumber3 = TicketCounter.builder()
                .currentNumber(3L)
                .id(UUID.randomUUID())
                .deleted(false)
                .version(2)
                .build();
    }

    @Test
    void getTicketNumber_shouldGetFirstNumber_whenEntityNotExistInDatabase () {
        when(repository.findById(any())).thenReturn(Optional.empty());
        when(validator.beforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(repository.create(any())).thenReturn(ticketCounterWithNumber1);

        when(validator.beforeUpdate(any())).thenReturn(true);
        when(repository.existsById(any())).thenReturn(true);
        when(repository.update(any())).thenReturn(ticketCounterWithNumber2);

        Long expectedTicketNumber = 1L;
        Long actualTicketNumber = service.getTicketNumber(UUID.randomUUID());
        assertEquals(expectedTicketNumber, actualTicketNumber);

        verify(repository, times(1)).findById(any());
        verify(validator, times(1)).beforeCreate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(repository, times(1)).create(any());

        verify(validator, times(1)).beforeUpdate(any());
        verify(repository, times(1)).existsById(any());
        verify(repository, times(1)).update(any());

    }

    @Test
    void getTicketNumber_shouldGetSecondNumber_whenEntityExistInDatabase () {
        when(repository.findById(any())).thenReturn(Optional.of(ticketCounterWithNumber2));

        when(validator.beforeUpdate(any())).thenReturn(true);
        when(repository.existsById(any())).thenReturn(true);
        when(repository.update(any())).thenReturn(ticketCounterWithNumber3);

        Long expectedTicketNumber = 2L;
        Long actualTicketNumber = service.getTicketNumber(UUID.randomUUID());
        assertEquals(expectedTicketNumber, actualTicketNumber);

        verify(repository, times(1)).findById(any());
        verify(validator, times(0)).beforeCreate(any());
        verify(validator, times(0)).checkUniqueness(any());
        verify(repository, times(0)).create(any());

        verify(validator, times(1)).beforeUpdate(any());
        verify(repository, times(1)).existsById(any());
        verify(repository, times(1)).update(any());

    }

}