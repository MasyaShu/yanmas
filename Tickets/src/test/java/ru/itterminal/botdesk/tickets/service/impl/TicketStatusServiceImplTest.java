package ru.itterminal.botdesk.tickets.service.impl;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.security.jwt.JwtUserBuilder;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.test.TicketStatusTestHelper;
import ru.itterminal.botdesk.tickets.repository.TicketStatusRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketStatusOperationValidator;

import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketStatusServiceImpl.class})
class TicketStatusServiceImplTest {

    @MockBean
    private TicketStatusOperationValidator validator;

    @MockBean
    private TicketStatusRepository repository;

    @MockBean
    private AccountServiceImpl accountService;

    @SuppressWarnings("unused")
    @MockBean
    private JwtUserBuilder jwtUserBuilder;

    @SuppressWarnings("unused")
    @MockBean
    private JwtUser jwtUser;

    @Autowired
    private TicketStatusServiceImpl service;

    private final TicketStatusTestHelper ticketStatusTestHelper = new TicketStatusTestHelper();

    @Test
    void createPredefinedEntity_shouldNoCreatePredefinedStatus_whenStatusInDataBase() {
        TicketStatus ticketStatus = ticketStatusTestHelper.getRandomValidEntity();
        when(repository.getByIsStartedPredefinedTrueAndAccount_Id(any())).thenReturn(Optional.of(ticketStatus));
        when(repository.getByIsReopenedPredefinedTrueAndAccount_Id(any())).thenReturn(Optional.of(ticketStatus));
        when(repository.getByIsFinishedPredefinedTrueAndAccount_Id(any())).thenReturn(Optional.of(ticketStatus));
        when(repository.getByIsCanceledPredefinedTrueAndAccount_Id(any())).thenReturn(Optional.of(ticketStatus));
        service.actionAfterCompletedVerificationAccount(any());
        verify(validator, times(0)).beforeCreate(any());
        verify(validator, times(0)).checkUniqueness(any());
        verify(repository, times(0)).create(any());
        verify(validator, times(0)).beforeCreate(any());
        verify(repository, times(1)).getByIsStartedPredefinedTrueAndAccount_Id(any());
        verify(repository, times(1)).getByIsReopenedPredefinedTrueAndAccount_Id(any());
        verify(repository, times(1)).getByIsFinishedPredefinedTrueAndAccount_Id(any());
        verify(repository, times(1)).getByIsCanceledPredefinedTrueAndAccount_Id(any());
    }

    @Test
    void createPredefinedEntity_shouldCreatePredefinedStatus_whenStatusNotInDataBase() {
        TicketStatus ticketStatus = ticketStatusTestHelper.getRandomValidEntity();
        when(validator.beforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(repository.create(any())).thenReturn(ticketStatus);
        when(accountService.findById(any())).thenReturn(ticketStatus.getAccount());
        when(repository.getByIsStartedPredefinedTrueAndAccount_Id(any())).thenThrow(EntityNotExistException.class);
        when(repository.getByIsReopenedPredefinedTrueAndAccount_Id(any())).thenThrow(EntityNotExistException.class);
        when(repository.getByIsFinishedPredefinedTrueAndAccount_Id(any())).thenThrow(EntityNotExistException.class);
        when(repository.getByIsCanceledPredefinedTrueAndAccount_Id(any())).thenThrow(EntityNotExistException.class);
        service.actionAfterCompletedVerificationAccount(any());
        verify(validator, times(4)).beforeCreate(any());
        verify(validator, times(4)).checkUniqueness(any());
        verify(repository, times(4)).create(any());
        verify(repository, times(1)).getByIsStartedPredefinedTrueAndAccount_Id(any());
        verify(repository, times(1)).getByIsReopenedPredefinedTrueAndAccount_Id(any());
        verify(repository, times(1)).getByIsFinishedPredefinedTrueAndAccount_Id(any());
        verify(repository, times(1)).getByIsCanceledPredefinedTrueAndAccount_Id(any());
    }

}