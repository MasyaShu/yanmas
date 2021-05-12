package ru.itterminal.yanmas.tickets.service.impl;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.aau.service.impl.AccountServiceImpl;
import ru.itterminal.yanmas.commons.exception.EntityNotExistException;
import ru.itterminal.yanmas.security.jwt.JwtUser;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;
import ru.itterminal.yanmas.tickets.model.TicketType;
import ru.itterminal.yanmas.tickets.model.test.TicketTypeTestHelper;
import ru.itterminal.yanmas.tickets.repository.TicketTypeRepository;
import ru.itterminal.yanmas.tickets.service.validator.TicketTypeOperationValidator;

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
    @Qualifier("basicOperationValidatorImpl")
    TicketTypeOperationValidator validator;

    @SuppressWarnings("unused")
    @MockBean
    private JwtUserBuilder jwtUserBuilder;

    @SuppressWarnings("unused")
    @MockBean
    private JwtUser jwtUser;

    private final TicketTypeTestHelper ticketTypeTestHelper = new TicketTypeTestHelper();

    @Test
    void createPredefinedEntity_shouldNoCreatePredefinedStatus_whenStatusInDataBase() {
        TicketType ticketType = ticketTypeTestHelper.getRandomValidEntity();
        when(repository.getByIsPredefinedForNewTicketTrueAndAccount_Id(any())).thenReturn(Optional.of(ticketType));
        service.actionAfterCompletedVerificationAccount(any(), null);
        verify(validator, times(0)).logicalValidationBeforeCreate(any());
        verify(validator, times(0)).checkUniqueness(any());
        verify(repository, times(0)).create(any());
        verify(validator, times(0)).logicalValidationBeforeCreate(any());
        verify(repository, times(1)).getByIsPredefinedForNewTicketTrueAndAccount_Id(any());
    }

    @Test
    void createPredefinedEntity_shouldCreatePredefinedStatus_whenStatusNotInDataBase() {
        TicketType ticketType = ticketTypeTestHelper.getRandomValidEntity();
        when(validator.logicalValidationBeforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(repository.create(any())).thenReturn(ticketType);
        when(accountService.findById(any())).thenReturn(ticketType.getAccount());
        when(repository.getByIsPredefinedForNewTicketTrueAndAccount_Id(any())).thenThrow(EntityNotExistException.class);
        service.actionAfterCompletedVerificationAccount(any(), null);
        verify(validator, times(1)).logicalValidationBeforeCreate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(repository, times(1)).create(any());
        verify(repository, times(1)).getByIsPredefinedForNewTicketTrueAndAccount_Id(any());
    }
}
