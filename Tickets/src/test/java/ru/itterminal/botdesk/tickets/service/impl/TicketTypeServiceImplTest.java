package ru.itterminal.botdesk.tickets.service.impl;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.test.TicketTypeTestHelper;
import ru.itterminal.botdesk.tickets.repository.TicketTypeRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketTypeOperationValidator;

import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.*;
import static ru.itterminal.botdesk.commons.service.CrudServiceWithAccount.FIND_INVALID_MESSAGE_WITH_ACCOUNT;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketTypeServiceImpl.class})
class TicketTypeServiceImplTest {

    @MockBean
    private TicketTypeRepository ticketTypeRepository;

    @MockBean
    private TicketTypeOperationValidator validator;

    @Autowired
    private TicketTypeServiceImpl service;

    private final TicketTypeTestHelper ticketTypeTestHelper = new TicketTypeTestHelper();

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserIsNull() {
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(null));
        verify(ticketTypeRepository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserEmailIsNull() {
        TicketType ticketType = ticketTypeTestHelper.getRandomValidEntity();
        ticketType.setName(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(ticketType));
        verify(ticketTypeRepository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserIdIsNull() {
        TicketType ticketType = ticketTypeTestHelper.getRandomValidEntity();
        ticketType.setId(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(ticketType));
        verify(ticketTypeRepository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserAccountIsNull() {
        TicketType ticketType = ticketTypeTestHelper.getRandomValidEntity();
        ticketType.setAccount(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(ticketType));
        verify(ticketTypeRepository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }


    @Test
    void update_shouldUpdateUser_whenPassedValidData() {
        TicketType ticketType = ticketTypeTestHelper.getRandomValidEntity();
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(ticketTypeRepository.existsById(any())).thenReturn(true);
        when(ticketTypeRepository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(ticketType));
        when(ticketTypeRepository.update(any())).thenReturn(ticketType);
        TicketType createdUser = service.update(ticketType);
        assertEquals(createdUser, ticketType);
        verify(validator, times(1)).beforeUpdate(any());
        verify(ticketTypeRepository, times(2)).existsById(any());
        verify(ticketTypeRepository, times(1)).findByIdAndAccountId(any(), any());
        verify(ticketTypeRepository, times(1)).update(any());
    }

    @Test
    void update_shouldGetEntityNotExistException_whenUserIdNotExistInDatabase() {
        TicketType ticketType = ticketTypeTestHelper.getRandomInvalidEntity();
        when(ticketTypeRepository.existsById(any())).thenReturn(false);
        when(ticketTypeRepository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(ticketType));
        when(ticketTypeRepository.update(any())).thenReturn(ticketType);
        Throwable throwable = assertThrows(EntityNotExistException.class, () -> service.update(ticketType));
        assertEquals(String.format(FIND_INVALID_MESSAGE_WITH_ACCOUNT, "id and accountId", ticketType.getId(), ticketType.getAccount().getId()), throwable.getMessage());
        verify(ticketTypeRepository, times(1)).existsById(any());
        verify(ticketTypeRepository, times(0)).findByIdAndAccountId(any(), any());
        verify(ticketTypeRepository, times(0)).update(any());
    }
}