package ru.itterminal.botdesk.tickets.model.test;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import ru.itterminal.botdesk.tickets.model.TicketSetting;

class TicketSettingTestHelperTest {

    private TicketSettingTestHelper helper = new TicketSettingTestHelper();

    @Test
    void method_should_when () {
        TicketSetting ticketSetting = helper.getRandomValidEntity();
        assertNotNull(ticketSetting);
    }
}