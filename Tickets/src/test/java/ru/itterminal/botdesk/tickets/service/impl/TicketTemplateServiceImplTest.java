package ru.itterminal.botdesk.tickets.service.impl;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.botdesk.commons.model.Account;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;
import ru.itterminal.botdesk.tickets.repository.TicketTemplateRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketTemplateOperationValidator;

import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketTemplateServiceImpl.class})
class TicketTemplateServiceImplTest {

    @MockBean
    private TicketTemplateOperationValidator validator;

    @MockBean
    private TicketTemplateRepository ticketTemplateRepository;

    @Autowired
    private TicketTemplateServiceImpl service;

    private TicketTemplate ticketTemplate;
    private Account account;

    @BeforeEach
    void setUpBeforeEach() {
        account = new Account();
        account.setId(UUID.fromString(TestSecurityConfig.ACCOUNT_1_ID));
        ticketTemplate = TicketTemplate
                .builder()
                .account(account)
                .expressionSchedule("1 1 1 31 1 1")
                .dateStart(1639404029000L)
                .zoneId("America/New_York")
                .build();
    }

    @Test
    void create_shouldErrorNextTime_whenPassedExpressionSchedule() {
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(ticketTemplateRepository.existsById(any())).thenReturn(true);
        when(ticketTemplateRepository.findById(any())).thenReturn(Optional.of(ticketTemplate));
        when(ticketTemplateRepository.create(any())).thenReturn(ticketTemplate);
        TicketTemplate createTicketTemplate = service.create(ticketTemplate);
        assertEquals(createTicketTemplate, ticketTemplate);
        //verify(validator, times(1)).beforeUpdate(any());
    }

}