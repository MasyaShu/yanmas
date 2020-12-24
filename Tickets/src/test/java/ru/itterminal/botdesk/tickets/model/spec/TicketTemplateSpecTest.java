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
import ru.itterminal.botdesk.tickets.model.TicketTemplate;
import ru.itterminal.botdesk.tickets.repository.TicketRepositoryTestConfig;
import ru.itterminal.botdesk.tickets.repository.TicketTemplateRepository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {TicketRepositoryTestConfig.class, TicketTemplateSpec.class})
@Sql({"/create-ticket-test.sql"})
class TicketTemplateSpecTest {

    @Autowired
    private TicketTemplateRepository repository;

    @Autowired
    TicketTemplateSpec spec;

    public static final String SUBJECT = "subject";
    private final Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "subject"));
    private Page<TicketTemplate> foundTicketTemplate;


    @Test
    void getTicketTemplateBySubjectSpec_shouldGetFiveEntity_whenSubjectExistInDatabase() {
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateBySubjectSpec("5454654545454"));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);

        assertTrue(foundTicketTemplate.getContent().isEmpty());
    }

    @Test
    void getTicketTemplatesByIsActiveSpec_shouldGetEmptyList_whenUserNotExistInDatabaseWithIsArchivedIsTrue() {
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplatesByIsActiveSpec(true));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        assertEquals(3, foundTicketTemplate.getContent().size());
    }
}