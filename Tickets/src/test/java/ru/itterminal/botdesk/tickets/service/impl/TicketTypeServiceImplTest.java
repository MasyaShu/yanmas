package ru.itterminal.botdesk.tickets.service.impl;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.test.TicketTypeTestHelper;
import ru.itterminal.botdesk.tickets.repository.TicketTypeRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketTypeOperationValidator;

import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@SpringJUnitConfig(value = {TicketTypeServiceImpl.class})
class TicketTypeServiceImplTest {

    @Autowired
    TicketTypeServiceImpl service;

    @MockBean
    TicketTypeRepository repository;

    @MockBean
    private AccountServiceImpl accountService;

    @MockBean
    TicketTypeOperationValidator validator;

    private final TicketTypeTestHelper ticketTypeTestHelper = new TicketTypeTestHelper();


    @Test
    void createPredefinedEntity_shouldNoCreatePredefinedStatus_whenStatusInDataBase() {
        TicketType ticketType = ticketTypeTestHelper.getRandomValidEntity();
        when(repository.getByIsPredefinedForNewTicketTrueAndAccount_Id(any())).thenReturn(Optional.of(ticketType));
        service.createPredefinedEntity(any());
        verify(validator, times(0)).beforeCreate(any());
        verify(validator, times(0)).checkUniqueness(any());
        verify(repository, times(0)).create(any());
        verify(validator, times(0)).beforeCreate(any());
        verify(repository, times(1)).getByIsPredefinedForNewTicketTrueAndAccount_Id(any());
    }

    @Test
    void createPredefinedEntity_shouldCreatePredefinedStatus_whenStatusNotInDataBase() {
        TicketType ticketType = ticketTypeTestHelper.getRandomValidEntity();
        when(validator.beforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(repository.create(any())).thenReturn(ticketType);
        when(accountService.findById(any())).thenReturn(ticketType.getAccount());
        when(repository.getByIsPredefinedForNewTicketTrueAndAccount_Id(any())).thenThrow(EntityNotExistException.class);
        service.createPredefinedEntity(any());
        verify(validator, times(1)).beforeCreate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(repository, times(1)).create(any());
        verify(repository, times(1)).getByIsPredefinedForNewTicketTrueAndAccount_Id(any());
    }
}