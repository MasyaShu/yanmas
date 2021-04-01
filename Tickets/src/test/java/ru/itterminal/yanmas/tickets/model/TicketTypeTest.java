package ru.itterminal.yanmas.tickets.model;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

@TestInstance(PER_CLASS)
class TicketTypeTest {

    TicketType ticketType_1;
    TicketType ticketType_2;
    final UUID id = UUID.randomUUID();

    @BeforeEach
    void setUp() {
        ticketType_1 = TicketType
                .builder()
                .account(null)
                .comment("comment")
                .name("ticketTypes_1")
                .build();
        ticketType_1.setId(id);
        ticketType_1.setDeleted(false);

        ticketType_2 = TicketType
                .builder()
                .account(null)
                .comment("comment")
                .name("ticketTypes_1")
                .build();
        ticketType_2.setId(id);
        ticketType_2.setDeleted(false);
    }

    @Test
    void equals_shouldGetTrue_whenAllFieldsAreEquals() {
        assertEquals(ticketType_1, ticketType_2);
    }

    @Test
    void equals_shouldGetFalse_whenOneFieldAreNotEquals() {
        ticketType_1.setComment("");
        assertNotEquals(ticketType_1, ticketType_2);
    }

    @Test
    void equals_shouldGetTrue_whenBothObjectsIsNew() {
        TicketType ticketType_1 = new TicketType();
        TicketType ticketType_2 = new TicketType();
        assertEquals(ticketType_1, ticketType_2);
    }

    @SuppressWarnings("deprecation")
    @Test
    void equals_shouldGetFalse_whenVersionNotEquals() {
        ticketType_1.setVersion(1);
        assertNotEquals(ticketType_1, ticketType_2);
    }


}