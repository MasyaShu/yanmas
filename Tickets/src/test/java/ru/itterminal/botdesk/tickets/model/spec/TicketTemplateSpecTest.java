package ru.itterminal.botdesk.tickets.model.spec;

import org.assertj.core.api.Assertions;
import org.hibernate.SessionFactory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.boot.test.autoconfigure.orm.jpa.TestEntityManager;
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
import java.util.UUID;
import java.util.stream.Collectors;

import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;
import static ru.itterminal.botdesk.tickets.model.test.TicketTemplateTestHelper.*;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {TicketRepositoryTestConfig.class, TicketTemplateSpec.class})
@Sql({"/create-ticket-test.sql"})
class TicketTemplateSpecTest {

    private static final String GE = ">=";
    private static final String GT = ">";
    private static final String LE = "<=";
    private static final String LT = "<";
    private static final String EQUALS = "=";
    private static final String DESCRIPTION = "description";
    private static final String DESCRIPTION_2 = "description_2";
    @Autowired
    private TicketTemplateRepository repository;

    @Autowired
    TicketTemplateSpec spec;

    @Autowired
    private TestEntityManager em;

    private static final String SUBJECT = "subject";
    private final Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "subject"));
    private Page<TicketTemplate> foundTicketTemplate;
    private final TicketTemplateTestHelper ticketTemplateTestHelper = new TicketTemplateTestHelper();


    @Test
    void getTicketTemplateBySubjectSpec_shouldGetEntitySubjectContainsString_whenSubjectEqualSubject() {
        SessionFactory sessionFactory = em.getEntityManager().getEntityManagerFactory()
                .unwrap(SessionFactory.class);
        sessionFactory.getStatistics().setStatisticsEnabled(true);

        List<TicketTemplate> ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateBySubjectSpec(SUBJECT));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(ticketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(ticketTemplates);
        Assertions.assertThat(sessionFactory.getStatistics().getPrepareStatementCount()).isEqualTo(2);
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
    void getTicketTemplateBySubjectSpec_shouldGetAllEntity_whenSubjectEmptyString() {
        List<TicketTemplate> ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateBySubjectSpec(BaseSpec.EMPTY_STRING));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(ticketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(ticketTemplates);
    }

    @Test
    void getTicketTemplateBySubjectSpec_shouldGetEmptyList_whenSubjectNotExistInDatabase() {
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateBySubjectSpec("not exist subject"));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        assertEquals(0, foundTicketTemplate.getContent().size());
    }

    @Test
    void getTicketTemplateByDescriptionSpec_shouldGetEntityDescriptionContains_whenDescriptionEqualsDescription() {
        List<TicketTemplate> ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateStart() != null && tt.getDescription().contains(DESCRIPTION))
                .collect(Collectors.toList());
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateByDescriptionSpec(DESCRIPTION));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void getTicketTemplateByDescriptionSpec_shouldGetEntity_whenDescriptionExistInDatabase() {
        List<TicketTemplate> ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDescription() != null && tt.getDescription().equals(DESCRIPTION_2))
                .collect(Collectors.toList());
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateByDescriptionSpec(DESCRIPTION_2));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void getTicketTemplateByDescriptionSpec_shouldGetAllEntity_whenDescriptionEmptyString() {
        List<TicketTemplate> ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateStart() == null || tt.getDescription().equals(BaseSpec.EMPTY_STRING))
                .collect(Collectors.toList());
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateByDescriptionSpec(BaseSpec.EMPTY_STRING));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void getTicketTemplateByDescriptionSpec_shouldGetEmptyList_whenSDescriptionNotExistInDatabase() {
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateByDescriptionSpec("not exist Description"));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        assertEquals(0, foundTicketTemplate.getContent().size());
    }

    @Test
    void getTicketTemplateByDateStartSpec_shouldEntityWithDateStartNotNullAndGreatThan_whenDate20200101() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateStart() != null && tt.getDateStart() > DATE_2020_01_01)
                .collect(Collectors.toList());
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateByDateStartSpec(DATE_2020_01_01, GT));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void getTicketTemplateByDateStartSpec_shouldEntityWithDateStartNotNullAndGreatEquals_whenDate20200101() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateStart() != null && tt.getDateStart() >= DATE_2020_01_01)
                .collect(Collectors.toList());
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateByDateStartSpec(DATE_2020_01_01, GE));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void getTicketTemplateByDateStartSpec_shouldEntityWithDateStartNullAndLessEquals_whenDate20190101() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateStart() == null || tt.getDateStart() <= DATE_2019_01_01)
                .collect(Collectors.toList());
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateByDateStartSpec(DATE_2019_01_01, LE));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void getTicketTemplateByDateStartSpec_shouldEntityWithDateStartNullAndLessThan_whenDate20190101() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateStart() == null || tt.getDateStart() < DATE_2019_01_01)
                .collect(Collectors.toList());
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateByDateStartSpec(DATE_2019_01_01, LT));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void getTicketTemplateByDateStartSpec_shouldEntityWithDateStartNullAndEquals_whenDate20190101() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateStart() != null && tt.getDateStart() == DATE_2019_01_01)
                .collect(Collectors.toList());
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateByDateStartSpec(DATE_2019_01_01, EQUALS));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void getTicketTemplateByDateEndSpec_shouldEntityWithDateEndNullAndGreatThan_whenDate20200101() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateEnd() == null || tt.getDateEnd() > DATE_2020_01_01)
                .collect(Collectors.toList());
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateByDateEndSpec(DATE_2020_01_01, GT));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void getTicketTemplateByDateEndSpec_shouldEntityWithDateEndNullAndGreatEquals_whenDate20200101() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateEnd() == null || tt.getDateEnd() >= DATE_2020_01_01)
                .collect(Collectors.toList());
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateByDateEndSpec(DATE_2020_01_01, GE));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void getTicketTemplateByDateEndSpec_shouldEntityWithDateEndNotNullAndLessEquals_whenDate20190101() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateEnd() != null && tt.getDateEnd() <= DATE_2019_01_01)
                .collect(Collectors.toList());
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateByDateEndSpec(DATE_2019_01_01, LE));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void getTicketTemplateByDateEndSpec_shouldEntityWithDateEndNotNullAndLessThan_whenDate20190101() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateEnd() != null && tt.getDateEnd() < DATE_2019_01_01)
                .collect(Collectors.toList());
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateByDateEndSpec(DATE_2019_01_01, LT));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void getTicketTemplateByDateEndSpec_shouldEntityWithDateEndNullAndEquals_whenDate20190101() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateEnd() != null && tt.getDateEnd() == DATE_2019_01_01)
                .collect(Collectors.toList());
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateByDateEndSpec(DATE_2019_01_01, EQUALS));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    } 

    @Test
    void getTicketTemplatesByIsOnlyOneTicketInWorkSpec_shouldGetEntity_whenIsOnlyOneTicketInWorkTrue() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(TicketTemplate::getIsOnlyOneTicketInWork)
                .collect(Collectors.toList());
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplatesByIsOnlyOneTicketInWorkSpec(true));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void getTicketTemplatesByIsOnlyOneTicketInWorkSpec_shouldGetEntity_whenIsOnlyOneTicketInWorkFalse() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt -> !tt.getIsOnlyOneTicketInWork())
                .collect(Collectors.toList());
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplatesByIsOnlyOneTicketInWorkSpec(false));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void getTicketTemplatesByIsActiveSpec_shouldGetEntity_whenIsActiveTrue() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(TicketTemplate::getIsActive)
                .collect(Collectors.toList());
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplatesByIsActiveSpec(true));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void getTicketTemplatesByIsActiveSpec_shouldGetEntity_whenIsActiveFalse() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt -> !tt.getIsActive())
                .collect(Collectors.toList());
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplatesByIsActiveSpec(false));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void getTicketTemplateByListOfAuthorsSpec_shouldGetAllEntity_whenListAllUUIDInDataBase() {
        List<UUID> listAuthorId = List.of(UUID.fromString(AUTHOR_ID_1), UUID.fromString(AUTHOR_ID_2));
        var expectedTicketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateByListOfAuthorsSpec(listAuthorId));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void getTicketTemplateByListOfAuthorsSpec_shouldGetEntity_whenListOneUUIDInDataBase() {
        List<UUID> listAuthorId = List.of(UUID.fromString(AUTHOR_ID_1));
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt -> tt.getAuthor().getId().equals(UUID.fromString(AUTHOR_ID_1)))
                .collect(Collectors.toList());
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateByListOfAuthorsSpec(listAuthorId));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void getTicketTemplateByListOfTicketTypeSpec_shouldGetEntityNotNull_whenListAllUUIDInDataBase() {
        List<UUID> listAuthorId = List.of(UUID.fromString(TICKET_TYPE_ID_1), UUID.fromString(TICKET_TYPE_ID_2), UUID.fromString(TICKET_TYPE_ID_3));
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt -> tt.getTicketType() != null)
                .collect(Collectors.toList());
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateByListOfTicketTypeSpec(listAuthorId));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void getTicketTemplateByListOfTicketTypeSpec_shouldGetEntity_whenListOneUUIDInDataBase() {
        List<UUID> listAuthorId = List.of(UUID.fromString(TICKET_TYPE_ID_1));
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt -> tt.getTicketType() != null && tt.getTicketType().getId().equals(UUID.fromString(TICKET_TYPE_ID_1)))
                .collect(Collectors.toList());
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateByListOfTicketTypeSpec(listAuthorId));
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void getTicketTemplateByListOfTicketTypeNullSpec_shouldGetEntityTicketTypeNull_whenListOneUUIDInDataBase() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt -> tt.getTicketType() == null)
                .collect(Collectors.toList());
        Specification<TicketTemplate> tSpecification = Specification
                .where(spec.getTicketTemplateByListOfTicketTypeNullSpec());
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }
}