package ru.itterminal.botdesk.tickets.model;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

@TestInstance(PER_CLASS)
class TicketTypesTest {

    TicketTypes ticketTypes_1;
    TicketTypes ticketTypes_2;
    final UUID id = UUID.randomUUID();

    @BeforeEach
    void setUp() {
        ticketTypes_1 = TicketTypes
                .builder()
                .account(null)
                .comment("comment")
                .name("ticketTypes_1")
                .build();
        ticketTypes_1.setId(id);
        ticketTypes_1.setDeleted(false);

        ticketTypes_2 = TicketTypes
                .builder()
                .account(null)
                .comment("comment")
                .name("ticketTypes_1")
                .build();
        ticketTypes_2.setId(id);
        ticketTypes_2.setDeleted(false);
    }

    @Test
    void equals_shouldGetTrue_whenAllFieldsAreEquals() {
        assertEquals(ticketTypes_1, ticketTypes_2);
    }

    @Test
    void equals_shouldGetFalse_whenOneFieldAreNotEquals() {
        ticketTypes_1.setComment("");
        assertNotEquals(ticketTypes_1, ticketTypes_2);
    }

    @Test
    void equals_shouldGetTrue_whenBothObjectsIsNew() {
        TicketTypes ticketTypes_1 = new TicketTypes();
        TicketTypes ticketTypes_2 = new TicketTypes();
        assertEquals(ticketTypes_1, ticketTypes_2);
    }

    @SuppressWarnings("deprecation")
    @Test
    void equals_shouldGetFalse_whenVersionNotEquals() {
        ticketTypes_1.setVersion(1);
        assertNotEquals(ticketTypes_1, ticketTypes_2);
    }


}