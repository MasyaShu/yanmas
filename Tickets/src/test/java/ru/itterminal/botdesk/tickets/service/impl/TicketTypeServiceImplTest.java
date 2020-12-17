package ru.itterminal.botdesk.tickets.service.impl;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.service.CrudService;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.repository.TicketTypeRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketTypeOperationValidator;

import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.*;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketTypeServiceImpl.class})
class TicketTypeServiceImplTest {

    @MockBean
    private TicketTypeRepository ticketTypeRepository;

    @MockBean
    private TicketTypeOperationValidator validator;

    @Autowired
    private TicketTypeServiceImpl service;

    private TicketType ticketType;
    private Account account;

    @BeforeEach
    void setUpBeforeEach() {
        account = new Account();
        account.setId(UUID.fromString(TestSecurityConfig.ACCOUNT_1_ID));
        ticketType = TicketType
                .builder()
                .account(account)
                .build();
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserIsNull() {
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(null));
        verify(ticketTypeRepository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserEmailIsNull() {
        ticketType.setName(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(ticketType));
        verify(ticketTypeRepository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserIdIsNull() {
        ticketType.setId(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(ticketType));
        verify(ticketTypeRepository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserAccountIsNull() {
        ticketType.setAccount(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(ticketType));
        verify(ticketTypeRepository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void findByIdAndAccountId_shouldGetEntityNotExistException_whenGroupIdIsNull() {
        UUID accountId = account.getId();
        assertThrows(EntityNotExistException.class, () -> service.findByIdAndAccountId(null, accountId));
        verify(ticketTypeRepository, times(0)).getByIdAndAccount_Id(any(), any());
    }

    @Test
    void findByIdAndAccountId_shouldGetEntityNotExistException_whenAccountIdIsNull() {
        UUID ticketTypeIdId = ticketType.getId();
        assertThrows(EntityNotExistException.class, () -> service.findByIdAndAccountId(ticketTypeIdId, null));
        verify(ticketTypeRepository, times(0)).getByIdAndAccount_Id(any(), any());
    }

    @Test
    void update_shouldUpdateUser_whenPassedValidData() {
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(ticketTypeRepository.existsById(any())).thenReturn(true);
        when(ticketTypeRepository.findById(any())).thenReturn(Optional.of(ticketType));
        when(ticketTypeRepository.update(any())).thenReturn(ticketType);
        TicketType createdUser = service.update(ticketType);
        assertEquals(createdUser, ticketType);
        verify(validator, times(1)).beforeUpdate(any());
        verify(ticketTypeRepository, times(2)).existsById(any());
        verify(ticketTypeRepository, times(1)).findById(any());
        verify(ticketTypeRepository, times(1)).update(any());
    }

    @Test
    void update_shouldGetEntityNotExistException_whenUserIdNotExistInDatabase() {
        when(ticketTypeRepository.existsById(any())).thenReturn(false);
        when(ticketTypeRepository.findById(any())).thenReturn(Optional.of(ticketType));
        when(ticketTypeRepository.update(any())).thenReturn(ticketType);
        Throwable throwable = assertThrows(EntityNotExistException.class, () -> service.update(ticketType));
        assertEquals(String.format(CrudService.FIND_INVALID_MESSAGE, "id", ticketType.getId()), throwable.getMessage());
        verify(ticketTypeRepository, times(1)).existsById(any());
        verify(ticketTypeRepository, times(0)).findById(any());
        verify(ticketTypeRepository, times(0)).update(any());
    }
}