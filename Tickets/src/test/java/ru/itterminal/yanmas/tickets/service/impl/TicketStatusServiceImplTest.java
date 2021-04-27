package ru.itterminal.yanmas.tickets.service.impl;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.aau.service.impl.AccountServiceImpl;
import ru.itterminal.yanmas.commons.exception.EntityNotExistException;
import ru.itterminal.yanmas.security.jwt.JwtUser;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;
import ru.itterminal.yanmas.tickets.model.TicketStatus;
import ru.itterminal.yanmas.tickets.model.test.TicketStatusTestHelper;
import ru.itterminal.yanmas.tickets.repository.TicketStatusRepository;
import ru.itterminal.yanmas.tickets.service.validator.TicketStatusOperationValidator;

import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketStatusServiceImpl.class})
class TicketStatusServiceImplTest {

    @MockBean
    @Qualifier("basicOperationValidatorImpl")
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
        verify(validator, times(0)).logicalValidationBeforeCreate(any());
        verify(validator, times(0)).checkUniqueness(any());
        verify(repository, times(0)).create(any());
        verify(validator, times(0)).logicalValidationBeforeCreate(any());
        verify(repository, times(1)).getByIsStartedPredefinedTrueAndAccount_Id(any());
        verify(repository, times(1)).getByIsReopenedPredefinedTrueAndAccount_Id(any());
        verify(repository, times(1)).getByIsFinishedPredefinedTrueAndAccount_Id(any());
        verify(repository, times(1)).getByIsCanceledPredefinedTrueAndAccount_Id(any());
    }

    @Test
    void createPredefinedEntity_shouldCreatePredefinedStatus_whenStatusNotInDataBase() {
        TicketStatus ticketStatus = ticketStatusTestHelper.getRandomValidEntity();
        when(validator.logicalValidationBeforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(repository.create(any())).thenReturn(ticketStatus);
        when(accountService.findById(any())).thenReturn(ticketStatus.getAccount());
        when(repository.getByIsStartedPredefinedTrueAndAccount_Id(any())).thenThrow(EntityNotExistException.class);
        when(repository.getByIsReopenedPredefinedTrueAndAccount_Id(any())).thenThrow(EntityNotExistException.class);
        when(repository.getByIsFinishedPredefinedTrueAndAccount_Id(any())).thenThrow(EntityNotExistException.class);
        when(repository.getByIsCanceledPredefinedTrueAndAccount_Id(any())).thenThrow(EntityNotExistException.class);
        service.actionAfterCompletedVerificationAccount(any());
        verify(validator, times(4)).logicalValidationBeforeCreate(any());
        verify(validator, times(4)).checkUniqueness(any());
        verify(repository, times(4)).create(any());
        verify(repository, times(1)).getByIsStartedPredefinedTrueAndAccount_Id(any());
        verify(repository, times(1)).getByIsReopenedPredefinedTrueAndAccount_Id(any());
        verify(repository, times(1)).getByIsFinishedPredefinedTrueAndAccount_Id(any());
        verify(repository, times(1)).getByIsCanceledPredefinedTrueAndAccount_Id(any());
    }

}
