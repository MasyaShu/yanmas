package ru.itterminal.botdesk.tickets.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Optional;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.tickets.model.test.TicketTestHelper;
import ru.itterminal.botdesk.tickets.repository.TicketRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator;

@SpringJUnitConfig(value = {TicketServiceImpl.class})
class TicketServiceImplTest {

    @MockBean
    private TicketRepository repository;

    @MockBean
    private TicketOperationValidator validator;

    @MockBean
    private TicketCounterServiceImpl counterService;

    @Autowired
    private TicketServiceImpl service;

    private final TicketTestHelper ticketTestHelper = new TicketTestHelper();

    @Test
    void create_shouldCreateTicket_whenPassedValidData() {
        var ticket = ticketTestHelper.getRandomValidEntity();
        when(validator.beforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(repository.create(any())).thenReturn(ticket);
        when(counterService.getTicketNumber(any())).thenReturn(ticket.getNumber());
        var createdTicket = service.create(ticket, ticket.getAuthor());
        assertEquals(ticket, createdTicket);
        verify(validator, times(1)).beforeCreate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(repository, times(1)).create(any());
        verify(counterService, times(1)).getTicketNumber(any());
    }

    @Test
    void create_shouldUpdateTicket_whenPassedValidData() {
        var ticket = ticketTestHelper.getRandomValidEntity();
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(repository.existsById(any())).thenReturn(true);
        when(repository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(ticket));
        when(repository.update(any())).thenReturn(ticket);
        var updatedTicket = service.update(ticket, ticket.getAuthor());
        assertEquals(ticket, updatedTicket);
        verify(validator, times(1)).beforeUpdate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(repository, times(1)).existsById(any());
        verify(repository, times(1)).findByIdAndAccountId(any(), any());
        verify(repository, times(1)).update(any());
        verify(counterService, times(0)).getTicketNumber(any());
    }
}