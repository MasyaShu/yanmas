package ru.itterminal.botdesk.tickets.service.validator;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.projection.TicketSettingUniqueFields;
import ru.itterminal.botdesk.tickets.service.impl.TicketSettingServiceImpl;

@SpringJUnitConfig(value = {TicketSettingOperationValidator.class})
class TicketSettingOperationValidatorTest {

    @MockBean
    private TicketSettingServiceImpl service;

    @Mock
    private TicketSettingUniqueFields ticketSettingUniqueFields;

    @Autowired
    private final  TicketSettingOperationValidator validator = new TicketSettingOperationValidator(service);

    private TicketSetting ticketSetting;
    private User observer1;
    private User observer2;
    private User executor1;
    private User executor2;
    private TicketType ticketTypeForNew;
    private TicketStatus ticketStatusForNew;
    private TicketStatus ticketStatusForReopen;
    private TicketStatus ticketStatusForClose;
    private TicketStatus ticketStatusForCancel;

    @BeforeEach
    void setUpBeforeEach() {
    }

    @Test
    void checkUniqueness_shouldGetTrue_whenTicketSettingIsUnique () {

    }

}