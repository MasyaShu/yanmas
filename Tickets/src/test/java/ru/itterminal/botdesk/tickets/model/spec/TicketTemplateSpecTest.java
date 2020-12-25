package ru.itterminal.botdesk.tickets.model.spec;

import org.assertj.core.api.Assertions;
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
import ru.itterminal.botdesk.commons.model.spec.BaseSpec;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;
import ru.itterminal.botdesk.tickets.model.test.TicketTemplateTestHelper;
import ru.itterminal.botdesk.tickets.repository.TicketRepositoryTestConfig;
import ru.itterminal.botdesk.tickets.repository.TicketTemplateRepository;

import java.util.List;

import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
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

    private static final String SUBJECT = "subject";
    private final Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "subject"));
    private Page<TicketTemplate> foundTicketTemplate;
    private final TicketTemplateTestHelper ticketTemplateTestHelper = new TicketTemplateTestHelper();


    @Test
    void getTicketTemplateBySubjectSpec_shouldGetFiveEntity_whenSubjectExistInDatabase() {
        List<TicketTemplate> ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateBySubjectSpec(SUBJECT));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(ticketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(ticketTemplates);
    }

    @Test
    void getTicketTemplateBySubjectSpec_shouldGetOneEntity_whenSubjectExistInDatabase() {
        TicketTemplate ticketTemplate = ticketTemplateTestHelper.getEntityFromPredefinedValidEntityByEntityId("f8a773d2-0f4d-48e9-b788-7ce671373992");
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateBySubjectSpec("subject_2"));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        assertEquals(1, foundTicketTemplate.getContent().size());
        assertEquals(ticketTemplate, foundTicketTemplate.getContent().get(0));
    }

    @Test
    void getTicketTemplateBySubjectSpec_shouldGetFiveEntity_whenSubjectEmptyString() {
        List<TicketTemplate> ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateBySubjectSpec(BaseSpec.EMPTY_STRING));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(ticketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(ticketTemplates);
    }

    @Test
    void getTicketTemplateBySubjectSpec_shouldGetEmptyList_whenSubjectNotExistInDatabase() {
        TicketTemplate ticketTemplate = ticketTemplateTestHelper.getEntityFromPredefinedValidEntityByEntityId("f8a773d2-0f4d-48e9-b788-7ce671373992");
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateBySubjectSpec("not exist subject"));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        assertEquals(0, foundTicketTemplate.getContent().size());
    }

    @Test
    void getTicketTemplatesByIsActiveSpec_shouldGetEmptyList_whenUserNotExistInDatabaseWithIsArchivedIsTrue() {
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplatesByIsActiveSpec(true));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        assertEquals(3, foundTicketTemplate.getContent().size());
    }
}