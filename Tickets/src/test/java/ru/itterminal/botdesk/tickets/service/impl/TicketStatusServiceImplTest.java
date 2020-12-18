package ru.itterminal.botdesk.tickets.service.impl;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.test.TicketStatusTestHelper;
import ru.itterminal.botdesk.tickets.repository.TicketStatusRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketStatusOperationValidator;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static ru.itterminal.botdesk.commons.service.CrudServiceWithAccount.FIND_INVALID_MESSAGE_WITH_ACCOUNT;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketStatusServiceImpl.class})
class TicketStatusServiceImplTest {

    @MockBean
    private TicketStatusRepository ticketStatusRepository;

    @MockBean
    private TicketStatusOperationValidator validator;

    @Autowired
    private TicketStatusServiceImpl service;

    private final TicketStatusTestHelper ticketSettingTestHelper = new TicketStatusTestHelper();


    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserIsNull() {
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(null));
        verify(ticketStatusRepository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserEmailIsNull() {
        TicketStatus ticketStatus = ticketSettingTestHelper.getRandomValidEntity();
        ticketStatus.setName(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(ticketStatus));
        verify(ticketStatusRepository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserIdIsNull() {
        TicketStatus ticketStatus = ticketSettingTestHelper.getRandomValidEntity();
        ticketStatus.setId(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(ticketStatus));
        verify(ticketStatusRepository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserAccountIsNull() {
        TicketStatus ticketStatus = ticketSettingTestHelper.getRandomValidEntity();
        ticketStatus.setAccount(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(ticketStatus));
        verify(ticketStatusRepository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }


    @Test
    void update_shouldUpdateUser_whenPassedValidData() {
        TicketStatus ticketStatus = ticketSettingTestHelper.getRandomValidEntity();
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(ticketStatusRepository.existsById(any())).thenReturn(true);
        when(ticketStatusRepository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(ticketStatus));
        when(ticketStatusRepository.update(any())).thenReturn(ticketStatus);
        TicketStatus createdUser = service.update(ticketStatus);
        assertEquals(createdUser, ticketStatus);
        verify(validator, times(1)).beforeUpdate(any());
        verify(ticketStatusRepository, times(2)).existsById(any());
        verify(ticketStatusRepository, times(1)).findByIdAndAccountId(any(), any());
        verify(ticketStatusRepository, times(1)).update(any());
    }

    @Test
    void update_shouldGetEntityNotExistException_whenUserIdNotExistInDatabase() {
        TicketStatus ticketStatus = ticketSettingTestHelper.getRandomInvalidEntity();
        when(ticketStatusRepository.existsById(any())).thenReturn(false);
        when(ticketStatusRepository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(ticketStatus));
        when(ticketStatusRepository.update(any())).thenReturn(ticketStatus);
        Throwable throwable = assertThrows(EntityNotExistException.class, () -> service.update(ticketStatus));
        assertEquals(String.format(FIND_INVALID_MESSAGE_WITH_ACCOUNT, "id and accountId", ticketStatus.getId(), ticketStatus.getAccount().getId()), throwable.getMessage());
        verify(ticketStatusRepository, times(1)).existsById(any());
        verify(ticketStatusRepository, times(0)).findByIdAndAccountId(any(), any());
        verify(ticketStatusRepository, times(0)).update(any());
    }

}