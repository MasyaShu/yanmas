package ru.itterminal.botdesk.tickets.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.Date;
import java.util.Optional;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.security.jwt.JwtUserBuilder;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;
import ru.itterminal.botdesk.tickets.model.test.TicketTemplateTestHelper;
import ru.itterminal.botdesk.tickets.repository.TicketTemplateRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketTemplateOperationValidator;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketTemplateServiceImpl.class})
class TicketTemplateServiceImplTest {

    @MockBean
    private TicketTemplateOperationValidator validator;

    @MockBean
    private TicketTemplateRepository ticketTemplateRepository;

    @SuppressWarnings("unused")
    @MockBean
    private JwtUserBuilder jwtUserBuilder;

    @SuppressWarnings("unused")
    @MockBean
    private JwtUser jwtUser;

    @Autowired
    private TicketTemplateServiceImpl service;

    private final TicketTemplateTestHelper ticketTemplateTestHelper = new TicketTemplateTestHelper();

    @Test
    void setNextExecutionTime_shouldSetDateNextRunTomorrow_whenDateStartTomorrow() {
        long startNowDay = TicketTemplateTestHelper.atStartOfDay(new Date()).getTime();
        TicketTemplate ticketTemplate = ticketTemplateTestHelper.getRandomValidEntity();
        ticketTemplate.setDateStart(startNowDay + 86400000L);
        ticketTemplate.setExpressionSchedule("* 10 * * * *");
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(ticketTemplateRepository.existsById(any())).thenReturn(true);
        when(ticketTemplateRepository.findById(any())).thenReturn(Optional.of(ticketTemplate));
        when(ticketTemplateRepository.create(any())).thenReturn(ticketTemplate);
        TicketTemplate createTicketTemplate = service.create(ticketTemplate);
        assertEquals(createTicketTemplate.getDateNextRun(), ticketTemplate.getDateStart() + 600000);
    }

    @Test
    void setNextExecutionTime_shouldSetDateNextRunToday_whenDateStartAfterCurrentData() {
        long startNowDay = TicketTemplateTestHelper.atStartOfDay(new Date()).getTime();
        long endNowDay = TicketTemplateTestHelper.atEndOfDay(new Date()).getTime() - 999;
        TicketTemplate ticketTemplate = ticketTemplateTestHelper.getRandomValidEntity();
        ticketTemplate.setDateStart(startNowDay);
        ticketTemplate.setExpressionSchedule("59 59 23 * * *");
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(ticketTemplateRepository.existsById(any())).thenReturn(true);
        when(ticketTemplateRepository.findById(any())).thenReturn(Optional.of(ticketTemplate));
        when(ticketTemplateRepository.create(any())).thenReturn(ticketTemplate);
        TicketTemplate createTicketTemplate = service.create(ticketTemplate);
        assertEquals(createTicketTemplate.getDateNextRun(), endNowDay);
    }

    @Test
    void setNextExecutionTime_shouldSetDateNextRunToday_whenDateStartNull() {
        long endNowDay = TicketTemplateTestHelper.atEndOfDay(new Date()).getTime() - 999;
        TicketTemplate ticketTemplate = ticketTemplateTestHelper.getRandomValidEntity();
        ticketTemplate.setExpressionSchedule("59 59 23 * * *");
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(ticketTemplateRepository.existsById(any())).thenReturn(true);
        when(ticketTemplateRepository.findById(any())).thenReturn(Optional.of(ticketTemplate));
        when(ticketTemplateRepository.create(any())).thenReturn(ticketTemplate);
        TicketTemplate createTicketTemplate = service.create(ticketTemplate);
        assertEquals(createTicketTemplate.getDateNextRun(), endNowDay);
    }

    @Test
    void setNextExecutionTime_shouldSetDateNextNull_whenDateEndBeforeFoundDateNextRun() {
        long startNowDay = TicketTemplateTestHelper.atStartOfDay(new Date()).getTime();
        TicketTemplate ticketTemplate = ticketTemplateTestHelper.getRandomValidEntity();
        ticketTemplate.setDateEnd(startNowDay);
        ticketTemplate.setExpressionSchedule("* 10 * * * *");
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(ticketTemplateRepository.existsById(any())).thenReturn(true);
        when(ticketTemplateRepository.findById(any())).thenReturn(Optional.of(ticketTemplate));
        when(ticketTemplateRepository.create(any())).thenReturn(ticketTemplate);
        TicketTemplate createTicketTemplate = service.create(ticketTemplate);
        assertNull(createTicketTemplate.getDateNextRun());
    }

    @Test
    void setNextExecutionTime_shouldGetNull_whenExpressionScheduleNotExecutable() {
        TicketTemplate ticketTemplate = ticketTemplateTestHelper.getRandomValidEntity();
        ticketTemplate.setExpressionSchedule("0 0 0 30 2 *");
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(ticketTemplateRepository.existsById(any())).thenReturn(true);
        when(ticketTemplateRepository.findById(any())).thenReturn(Optional.of(ticketTemplate));
        when(ticketTemplateRepository.create(any())).thenReturn(ticketTemplate);
        TicketTemplate createTicketTemplate = service.create(ticketTemplate);
        assertNull(createTicketTemplate.getDateNextRun());
    }
}