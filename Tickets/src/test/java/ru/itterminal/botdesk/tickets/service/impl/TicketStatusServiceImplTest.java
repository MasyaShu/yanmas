package ru.itterminal.botdesk.tickets.service.impl;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.repository.TicketStatusRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketStatusOperationValidator;

import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static ru.itterminal.botdesk.commons.service.CrudService.ENTITY_NOT_EXIST_MESSAGE;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketStatusServiceImpl.class})
class TicketStatusServiceImplTest {

    @MockBean
    private TicketStatusRepository ticketStatusRepository;

    @MockBean
    private TicketStatusOperationValidator validator;

    @Autowired
    private TicketStatusServiceImpl service;

    private TicketStatus ticketStatus;
    private Account account;

    @BeforeEach
    void setUpBeforeEach() {
        account = new Account();
        account.setId(UUID.fromString(TestSecurityConfig.ACCOUNT_1_ID));
        ticketStatus = TicketStatus
                .builder()
                .account(account)
                .build();
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserIsNull() {
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(null));
        verify(ticketStatusRepository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserEmailIsNull() {
        ticketStatus.setName(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(ticketStatus));
        verify(ticketStatusRepository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserIdIsNull() {
        ticketStatus.setId(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(ticketStatus));
        verify(ticketStatusRepository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserAccountIsNull() {
        ticketStatus.setAccount(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(ticketStatus));
        verify(ticketStatusRepository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void findByIdAndAccountId_shouldGetEntityNotExistException_whenGroupIdIsNull() {
        UUID accountId = account.getId();
        assertThrows(EntityNotExistException.class, () -> service.findByIdAndAccountId(null, accountId));
        verify(ticketStatusRepository, times(0)).getByIdAndAccount_Id(any(), any());
    }

    @Test
    void findByIdAndAccountId_shouldGetEntityNotExistException_whenAccountIdIsNull() {
        UUID ticketStatusIdId = ticketStatus.getId();
        assertThrows(EntityNotExistException.class, () -> service.findByIdAndAccountId(ticketStatusIdId, null));
        verify(ticketStatusRepository, times(0)).getByIdAndAccount_Id(any(), any());
    }

    @Test
    void update_shouldUpdateUser_whenPassedValidData() {
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(ticketStatusRepository.existsById(any())).thenReturn(true);
        when(ticketStatusRepository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(ticketStatus));
        when(ticketStatusRepository.update(any())).thenReturn(ticketStatus);
        TicketStatus createdUser = service.update(ticketStatus);
        assertEquals(createdUser, ticketStatus);
        verify(validator, times(1)).beforeUpdate(any());
        verify(ticketStatusRepository, times(1)).existsById(any());
        verify(ticketStatusRepository, times(1)).findByIdAndAccountId(any(), any());
        verify(ticketStatusRepository, times(1)).update(any());
    }

    @Test
    void update_shouldGetEntityNotExistException_whenUserIdNotExistInDatabase() {
        when(ticketStatusRepository.existsById(any())).thenReturn(false);
        when(ticketStatusRepository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(ticketStatus));
        when(ticketStatusRepository.update(any())).thenReturn(ticketStatus);
        Throwable throwable = assertThrows(EntityNotExistException.class, () -> service.update(ticketStatus));
        assertEquals(String.format(ENTITY_NOT_EXIST_MESSAGE, ticketStatus.getClass().getSimpleName(), ticketStatus.getId()), throwable.getMessage());
        verify(ticketStatusRepository, times(1)).existsById(any());
        verify(ticketStatusRepository, times(1)).findByIdAndAccountId(any(), any());
        verify(ticketStatusRepository, times(0)).update(any());
    }

}