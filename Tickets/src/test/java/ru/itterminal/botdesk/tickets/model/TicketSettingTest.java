package ru.itterminal.botdesk.tickets.model;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import ru.itterminal.botdesk.tickets.model.test.TicketSettingTestHelper;

class TicketSettingTest {

    private final TicketSettingTestHelper helper = new TicketSettingTestHelper();

    @Test
    void equals_shouldGetTrue_whenObjectsIsEqual() {
        TicketSetting ticketSetting1 = helper.getPredefinedValidEntityList().get(0);
        TicketSetting ticketSetting2 = helper.getPredefinedValidEntityList().get(0);
        assertEquals(ticketSetting1, ticketSetting2);
    }

    @Test
    void generateDisplayName_shouldGroupDisplayName_whenAuthorIsNull() {
        TicketSetting ticketSetting = helper.getPredefinedValidEntityList().get(0);
        ticketSetting.setAuthor(null);
        ticketSetting.generateDisplayName();
        assertEquals(ticketSetting.getGroup().getDisplayName(), ticketSetting.getDisplayName());
    }

    @Test
    void generateDisplayName_shouldAuthorDisplayName_whenGroupIsNull() {
        TicketSetting ticketSetting = helper.getPredefinedValidEntityList().get(0);
        ticketSetting.setGroup(null);
        ticketSetting.generateDisplayName();
        assertEquals(ticketSetting.getAuthor().getDisplayName(), ticketSetting.getDisplayName());
    }

    @Test
    void generateDisplayName_shouldGroupAndAuthorDisplayName_whenDataIsNotNull() {
        TicketSetting ticketSetting = helper.getPredefinedValidEntityList().get(0);
        ticketSetting.generateDisplayName();
        assertEquals(ticketSetting.getGroup().getDisplayName() + " " + ticketSetting.getAuthor().getDisplayName(),
                     ticketSetting.getDisplayName());
    }

}