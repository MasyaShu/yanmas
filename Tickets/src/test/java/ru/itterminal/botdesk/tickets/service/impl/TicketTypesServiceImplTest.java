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
import ru.itterminal.botdesk.tickets.model.TicketTypes;
import ru.itterminal.botdesk.tickets.repository.TicketTypesRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketTypesOperationValidator;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketTypesServiceImpl.class})
class TicketTypesServiceImplTest {

    @MockBean
    private TicketTypesRepository ticketTypesRepository;

    @MockBean
    private TicketTypesOperationValidator validator;

    @Autowired
    private TicketTypesServiceImpl service;

    private TicketTypes ticketTypes;
    private Account account;

    @BeforeEach
    void setUpBeforeEach() {
        account = new Account();
        account.setId(UUID.fromString(TestSecurityConfig.ACCOUNT_1_ID));
        ticketTypes = TicketTypes
                .builder()
                .account(account)
                .build();
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserIsNull() {
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(null));
        verify(ticketTypesRepository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserEmailIsNull() {
        ticketTypes.setName(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(ticketTypes));
        verify(ticketTypesRepository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserIdIsNull() {
        ticketTypes.setId(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(ticketTypes));
        verify(ticketTypesRepository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEntityNotExistException_whenUserAccountIsNull() {
        ticketTypes.setAccount(null);
        assertThrows(EntityNotExistException.class, () -> service.findByUniqueFields(ticketTypes));
        verify(ticketTypesRepository, times(0)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void findByIdAndAccountId_shouldGetEntityNotExistException_whenGroupIdIsNull() {
        UUID accountId = account.getId();
        assertThrows(EntityNotExistException.class, () -> service.findByIdAndAccountId(null, accountId));
        verify(ticketTypesRepository, times(0)).getByIdAndAccount_Id(any(), any());
    }

    @Test
    void findByIdAndAccountId_shouldGetEntityNotExistException_whenAccountIdIsNull() {
        UUID groupId = ticketTypes.getId();
        assertThrows(EntityNotExistException.class, () -> service.findByIdAndAccountId(groupId, null));
        verify(ticketTypesRepository, times(0)).getByIdAndAccount_Id(any(), any());
    }

}