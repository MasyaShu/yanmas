package ru.itterminal.botdesk.tickets.model.spec;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.repository.TicketRepositoryTestConfig;
import ru.itterminal.botdesk.tickets.repository.TicketStatusRepository;
import ru.itterminal.botdesk.tickets.repository.TicketTypeRepository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {TicketRepositoryTestConfig.class, TicketStatusSpec.class})
@Sql({"/create-ticket-test.sql"})
class TicketStatusSpecTest {

    @Autowired
    private TicketStatusRepository ticketTypeRepository;

    @Autowired
    TicketStatusSpec spec;
    
    private static final String TICKET_STATUS_1_NAME = "started";
    private static final String TICKET_STATUS_2_NAME = "reopened";
    private static final String TICKET_STATUS_3_NAME = "inWork";
    private static final String TICKET_STATUS_4_NAME = "finished";
    private static final String TICKET_STATUS_5_NAME = "canceled";
    private static final String TICKET_STATUS_NAME_ALL = "";
    private static final String TICKET_STATUS_NOT_EXIST = "NotExist";
    private final Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "sortIndex"));
    private Page<TicketStatus> foundTicketStatus;

    @Test
    void getTicketStatusByNameSpec_shouldGetOneTicketStatus_whenNameExistInDatabase() {
        Specification<TicketStatus> ticketStatusSpecification = Specification
                .where(spec.getTicketStatusByNameSpec(TICKET_STATUS_1_NAME.toUpperCase()));
        foundTicketStatus = ticketTypeRepository.findAll(ticketStatusSpecification, pageable);
        assertEquals(TICKET_STATUS_1_NAME, foundTicketStatus.getContent().get(0).getName());
    }

    @Test
    void getTicketStatusByNameSpec_shouldGetFiveTicketStatus_whenNameExistInDatabase() {
        Specification<TicketStatus> userSpecification = Specification
                .where(spec.getTicketStatusByNameSpec(TICKET_STATUS_NAME_ALL.toUpperCase()));
        foundTicketStatus = ticketTypeRepository.findAll(userSpecification, pageable);
        assertEquals(TICKET_STATUS_1_NAME, foundTicketStatus.getContent().get(0).getName());
        assertEquals(TICKET_STATUS_2_NAME, foundTicketStatus.getContent().get(1).getName());
        assertEquals(TICKET_STATUS_3_NAME, foundTicketStatus.getContent().get(2).getName());
        assertEquals(TICKET_STATUS_4_NAME, foundTicketStatus.getContent().get(3).getName());
        assertEquals(TICKET_STATUS_5_NAME, foundTicketStatus.getContent().get(4).getName());
        assertEquals(10, foundTicketStatus.getContent().get(0).getSortIndex());
        assertEquals(20, foundTicketStatus.getContent().get(1).getSortIndex());
        assertEquals(40, foundTicketStatus.getContent().get(2).getSortIndex());
        assertEquals(50, foundTicketStatus.getContent().get(3).getSortIndex());
        assertEquals(60, foundTicketStatus.getContent().get(4).getSortIndex());
        assertEquals(5, foundTicketStatus.getContent().size());
    }

    @Test
    void getTicketStatusByNameSpec_shouldGetEmptyList_whenNameNotExistInDatabase() {
        Specification<TicketStatus> userSpecification = Specification
                .where(spec.getTicketStatusByNameSpec(TICKET_STATUS_NOT_EXIST.toUpperCase()));
        foundTicketStatus = ticketTypeRepository.findAll(userSpecification, pageable);
        assertTrue(foundTicketStatus.getContent().isEmpty());
    }
}