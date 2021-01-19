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
import ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter;
import ru.itterminal.botdesk.commons.model.filter.NumberFilter;
import ru.itterminal.botdesk.commons.model.spec.BaseEntityFilterSpecificationsFactory;
import ru.itterminal.botdesk.commons.model.spec.NumberFilterSpecificationsFactory;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;
import ru.itterminal.botdesk.tickets.model.test.TicketTemplateTestHelper;
import ru.itterminal.botdesk.tickets.repository.TicketRepositoryTestConfig;
import ru.itterminal.botdesk.tickets.repository.TicketTemplateRepository;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;
import static ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;
import static ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.NOT_EXIST_IN;
import static ru.itterminal.botdesk.commons.model.filter.NumberFilter.TypeComparisonForNumberFilter.*;
import static ru.itterminal.botdesk.tickets.model.spec.TicketTemplateSpec.DATE_START;
import static ru.itterminal.botdesk.tickets.model.test.TicketTemplateTestHelper.*;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {TicketRepositoryTestConfig.class})
@Sql({"/create-ticket-test.sql"})
class BaseEntityFilterSpecificationsFactoryTest {

    private static final String TICKET_TYPE = "ticketType";
    @Autowired
    private TicketTemplateRepository repository;

    private final Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "subject"));
    private Page<TicketTemplate> foundTicketTemplate;
    private final TicketTemplateTestHelper ticketTemplateTestHelper = new TicketTemplateTestHelper();
    private final BaseEntityFilterSpecificationsFactory baseEntityFactory =
            new BaseEntityFilterSpecificationsFactory();


    @Test
    void isEmpty_shouldEntityWithTicketTypeNull_whenComparison_IS_EMPTY() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt -> tt.getTicketType() == null)
                .collect(Collectors.toList());
        var filter = BaseEntityFilter.builder()
                .typeComparison(IS_EMPTY.toString())
                .build();
        Specification<TicketTemplate> tSpecification =
                baseEntityFactory.makeSpecification(filter, TICKET_TYPE);
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void isNotEmpty_shouldEntityWithTicketTypeNotNull_whenComparison_IS_NOT_EMPTY() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt -> tt.getTicketType() != null)
                .collect(Collectors.toList());
        var filter = BaseEntityFilter.builder()
                .typeComparison(IS_NOT_EMPTY.toString())
                .build();
        Specification<TicketTemplate> tSpecification =
                baseEntityFactory.makeSpecification(filter, TICKET_TYPE);
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void existIn_shouldGetEntity_whenComparison_EXIST_IN() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt -> tt.getTicketType() != null
                        && (tt.getTicketType().getId().equals(UUID.fromString(TICKET_TYPE_ID_1))
                || tt.getTicketType().getId().equals(UUID.fromString(TICKET_TYPE_ID_2))))
                .collect(Collectors.toList());
        var filter = BaseEntityFilter.builder()
                .typeComparison(EXIST_IN.toString())
                .idEntity(List.of(UUID.fromString(TICKET_TYPE_ID_1), UUID.fromString(TICKET_TYPE_ID_2)))
                .build();
        Specification<TicketTemplate> tSpecification =
                baseEntityFactory.makeSpecification(filter, TICKET_TYPE);
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void notExistIn_shouldGetEntity_whenComparison_NOT_EXIST_IN() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt -> tt.getTicketType() != null
                        && !tt.getTicketType().getId().equals(UUID.fromString(TICKET_TYPE_ID_1))
                        && !tt.getTicketType().getId().equals(UUID.fromString(TICKET_TYPE_ID_2)))
                .collect(Collectors.toList());
        var filter = BaseEntityFilter.builder()
                .typeComparison(NOT_EXIST_IN.toString())
                .idEntity(List.of(UUID.fromString(TICKET_TYPE_ID_1), UUID.fromString(TICKET_TYPE_ID_2)))
                .build();
        Specification<TicketTemplate> tSpecification =
                baseEntityFactory.makeSpecification(filter, TICKET_TYPE);
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }


}
