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
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.repository.TicketTypeRepository;
import ru.itterminal.botdesk.tickets.repository.TicketTypeRepositoryTestConfig;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {TicketTypeRepositoryTestConfig.class, TicketTypeSpec.class})
@Sql({"/create-user-test.sql"})
class TicketTypeSpecTest {

    @Autowired
    private TicketTypeRepository ticketTypeRepository;

    @Autowired
    TicketTypeSpec spec;

    private static final String TICKET_TYPES_1_NAME = "ticketTypes1";
    private static final String TICKET_TYPES_2_NAME = "ticketTypes2";
    private static final String TICKET_TYPES_3_NAME = "ticketTypes3";
    private static final String TICKET_TYPES_4_NAME = "ticketTypes4";
    private static final String TICKET_TYPES_5_NAME = "ticketTypes5";
    private static final String TICKET_TYPES_NAME_ALL = "ticketTypes";
    private static final String TICKET_TYPES_NOT_EXIST = "NotExist";
    private static final String TICKET_TYPES_COMMENT_ALL = "comment";
private final Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "name"));
    private Page<TicketType> foundTicketTypes;


    @Test
    void getTicketTypesByNameSpec_shouldGetOneTicketTypes_whenNameExistInDatabase() {
        Specification<TicketType> userSpecification = Specification
                .where(spec.getTicketTypesByNameSpec(TICKET_TYPES_1_NAME.toUpperCase()));
        foundTicketTypes = ticketTypeRepository.findAll(userSpecification, pageable);
        assertEquals(TICKET_TYPES_1_NAME, foundTicketTypes.getContent().get(0).getName());
    }

    @Test
    void getTicketTypesByNameSpec_shouldGetFiveTicketTypes_whenNameExistInDatabase() {
        Specification<TicketType> userSpecification = Specification
                .where(spec.getTicketTypesByNameSpec(TICKET_TYPES_NAME_ALL.toUpperCase()));
        foundTicketTypes = ticketTypeRepository.findAll(userSpecification, pageable);
        assertEquals(TICKET_TYPES_1_NAME, foundTicketTypes.getContent().get(0).getName());
        assertEquals(TICKET_TYPES_2_NAME, foundTicketTypes.getContent().get(1).getName());
        assertEquals(TICKET_TYPES_3_NAME, foundTicketTypes.getContent().get(2).getName());
        assertEquals(TICKET_TYPES_4_NAME, foundTicketTypes.getContent().get(3).getName());
        assertEquals(TICKET_TYPES_5_NAME, foundTicketTypes.getContent().get(4).getName());
        assertEquals(5, foundTicketTypes.getContent().size());
    }

    @Test
    void getTicketTypesByNameSpec_shouldGetEmptyList_whenNameNotExistInDatabase() {
        Specification<TicketType> userSpecification = Specification
                .where(spec.getTicketTypesByNameSpec(TICKET_TYPES_NOT_EXIST.toUpperCase()));
        foundTicketTypes = ticketTypeRepository.findAll(userSpecification, pageable);
        assertTrue(foundTicketTypes.getContent().isEmpty());
    }

    @Test
    void getTicketTypesByCommentSpec_shouldGetFourTicketTypes_whenCommentExistInDatabase() {
        Specification<TicketType> userSpecification = Specification
                .where(spec.getTicketTypesByCommentSpec(TICKET_TYPES_COMMENT_ALL.toUpperCase()));
        foundTicketTypes = ticketTypeRepository.findAll(userSpecification, pageable);
        assertEquals(TICKET_TYPES_1_NAME, foundTicketTypes.getContent().get(0).getName());
        assertEquals(TICKET_TYPES_2_NAME, foundTicketTypes.getContent().get(1).getName());
        assertEquals(TICKET_TYPES_3_NAME, foundTicketTypes.getContent().get(2).getName());
        assertEquals(TICKET_TYPES_4_NAME, foundTicketTypes.getContent().get(3).getName());
        assertEquals(4, foundTicketTypes.getContent().size());
    }

    @Test
    void getTicketTypesByCommentSpec_shouldGetEmptyList_whenCommentNotExistInDatabase() {
        Specification<TicketType> userSpecification = Specification
                .where(spec.getTicketTypesByCommentSpec(TICKET_TYPES_NOT_EXIST.toUpperCase()));
        foundTicketTypes = ticketTypeRepository.findAll(userSpecification, pageable);
        assertTrue(foundTicketTypes.getContent().isEmpty());
    }

    @Test
    void getUserByCommentSpec_shouldGetOneTicketType_whenCommentIsEmpty() {
        Specification<TicketType> userSpecification = Specification
                .where(spec.getTicketTypesByCommentSpec(""));
        foundTicketTypes = ticketTypeRepository.findAll(userSpecification, pageable);
        assertEquals(TICKET_TYPES_5_NAME, foundTicketTypes.getContent().get(0).getName());
        assertEquals(1, foundTicketTypes.getContent().size());
    }

    @Test
    void getTicketTypesByIsPredefinedSpec_shouldGetTwoTicketType_whenIsPredefinedTrue() {
        Specification<TicketType> userSpecification = Specification
                .where(spec.getTicketTypesByIsPredefinedSpec(true));
        foundTicketTypes = ticketTypeRepository.findAll(userSpecification, pageable);
        assertEquals(TICKET_TYPES_1_NAME, foundTicketTypes.getContent().get(0).getName());
        assertEquals(TICKET_TYPES_3_NAME, foundTicketTypes.getContent().get(1).getName());
        assertEquals(2, foundTicketTypes.getContent().size());
    }

    @Test
    void getTicketTypesByIsPredefinedSpec_shouldGetTreeTicketType_whenIsPredefinedFalse() {
        Specification<TicketType> userSpecification = Specification
                .where(spec.getTicketTypesByIsPredefinedSpec(false));
        foundTicketTypes = ticketTypeRepository.findAll(userSpecification, pageable);
        assertEquals(TICKET_TYPES_2_NAME, foundTicketTypes.getContent().get(0).getName());
        assertEquals(TICKET_TYPES_4_NAME, foundTicketTypes.getContent().get(1).getName());
        assertEquals(TICKET_TYPES_5_NAME, foundTicketTypes.getContent().get(2).getName());
        assertEquals(3, foundTicketTypes.getContent().size());
    }
}