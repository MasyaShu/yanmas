package ru.itterminal.botdesk.tickets.model;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

@TestInstance(PER_CLASS)
class TicketTemplateTest {

    TicketTemplate ticketStatus_1;
    TicketTemplate ticketStatus_2;
    final UUID id = UUID.randomUUID();

    @BeforeEach
    void setUp() {
        ticketStatus_1 = TicketTemplate
                .builder()
                .account(null)
                .subject("TicketTemplate")
                .description("")
                .author(null)
                .dateEnd(0L)
                .dateNextRun(0L)
                .dateStart(0L)
                .expressionSchedule("1 2 3 4 5 6")
                .isActive(true)
                .isOnlyOneTicketInWork(true)
                .ticketType(null)
                .zoneId("Europe/Moscow")
                .build();
        ticketStatus_1.setId(id);
        ticketStatus_1.setDeleted(false);

        ticketStatus_2 = TicketTemplate
                .builder()
                .account(null)
                .subject("TicketTemplate")
                .description("")
                .author(null)
                .dateEnd(0L)
                .dateNextRun(0L)
                .dateStart(0L)
                .expressionSchedule("1 2 3 4 5 6")
                .isActive(true)
                .isOnlyOneTicketInWork(true)
                .ticketType(null)
                .zoneId("Europe/Moscow")
                .build();
        ticketStatus_2.setId(id);
        ticketStatus_2.setDeleted(false);
    }

    @Test
    void equals_shouldGetTrue_whenAllFieldsAreEquals() {
        assertEquals(ticketStatus_1, ticketStatus_2);
    }

    @Test
    void equals_shouldGetFalse_whenOneFieldAreNotEquals() {
        ticketStatus_1.setSubject("");
        assertNotEquals(ticketStatus_1, ticketStatus_2);
    }

    @Test
    void equals_shouldGetTrue_whenBothObjectsIsNew() {
        TicketTemplate ticketStatus_1 = new TicketTemplate();
        TicketTemplate ticketStatus_2 = new TicketTemplate();
        assertEquals(ticketStatus_1, ticketStatus_2);
    }

    @SuppressWarnings("deprecation")
    @Test
    void equals_shouldGetFalse_whenVersionNotEquals() {
        ticketStatus_1.setVersion(1);
        assertNotEquals(ticketStatus_1, ticketStatus_2);
    }

}