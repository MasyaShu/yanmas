package ru.itterminal.botdesk.tickets.service.impl;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;
import ru.itterminal.botdesk.tickets.repository.TicketTemplateRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketTemplateOperationValidator;

import java.util.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

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

    @BeforeEach
    void setUpBeforeEach() {
        Account account = new Account();
        account.setId(UUID.fromString(TestSecurityConfig.ACCOUNT_1_ID));
        Calendar calendar = new GregorianCalendar();
        calendar.set(Calendar.HOUR_OF_DAY, 0);
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        long currentTime = calendar.getTimeInMillis();
        calendar.get(Calendar.DAY_OF_MONTH);
        ticketTemplate = TicketTemplate
                .builder()
                .account(account)
                .expressionSchedule("0 0 0 14 * *")
                .dateStart(currentTime + 86400000L)
                .dateEnd(currentTime + 31556926000L)
                .zoneId("America/New_York")
                .build();
    }

    @Test
    void setNextExecutionTime_shouldSetDateNextRun_whenPassedExpressionSchedule() {
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(ticketTemplateRepository.existsById(any())).thenReturn(true);
        when(ticketTemplateRepository.findById(any())).thenReturn(Optional.of(ticketTemplate));
        when(ticketTemplateRepository.create(any())).thenReturn(ticketTemplate);
        TicketTemplate createTicketTemplate = service.create(ticketTemplate);
       // var dateNR = new Date(createTicketTemplate.getDateNextRun());
      //  assertEquals(createTicketTemplate, ticketTemplate);
        //verify(validator, times(1)).beforeUpdate(any());
    }

}