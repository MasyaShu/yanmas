package ru.itterminal.yanmas.tickets.model;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

@TestInstance(PER_CLASS)
class TicketStatusTest {
    
    TicketStatus ticketStatus_1;
    TicketStatus ticketStatus_2;
    final UUID id = UUID.randomUUID();

    @BeforeEach
    void setUp() {
        ticketStatus_1 = TicketStatus
                .builder()
                .account(null)
                .name("ticketStatus_1")
                .isCanceledPredefined(true)
                .isFinishedPredefined(false)
                .isReopenedPredefined(false)
                .isStartedPredefined(false)
                .sortIndex(10)
                .build();
        ticketStatus_1.setId(id);
        ticketStatus_1.setDeleted(false);

        ticketStatus_2 = TicketStatus
                .builder()
                .account(null)
                .name("ticketStatus_1")
                .isCanceledPredefined(true)
                .isFinishedPredefined(false)
                .isReopenedPredefined(false)
                .isStartedPredefined(false)
                .sortIndex(10)
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
        ticketStatus_1.setSortIndex(0);
        assertNotEquals(ticketStatus_1, ticketStatus_2);
    }

    @Test
    void equals_shouldGetTrue_whenBothObjectsIsNew() {
        TicketStatus ticketStatus_1 = new TicketStatus();
        TicketStatus ticketStatus_2 = new TicketStatus();
        assertEquals(ticketStatus_1, ticketStatus_2);
    }

    @SuppressWarnings("deprecation")
    @Test
    void equals_shouldGetFalse_whenVersionNotEquals() {
        ticketStatus_1.setVersion(1);
        assertNotEquals(ticketStatus_1, ticketStatus_2);
    }
}