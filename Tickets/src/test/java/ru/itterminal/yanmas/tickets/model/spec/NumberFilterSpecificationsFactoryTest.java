package ru.itterminal.yanmas.tickets.model.spec;


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
import ru.itterminal.yanmas.commons.model.filter.NumberFilter;
import ru.itterminal.yanmas.commons.model.spec.NumberFilterSpecificationsFactory;
import ru.itterminal.yanmas.tickets.model.TicketTemplate;
import ru.itterminal.yanmas.tickets.model.test.TicketTemplateTestHelper;
import ru.itterminal.yanmas.tickets.repository.TicketRepositoryTestConfig;
import ru.itterminal.yanmas.tickets.repository.TicketTemplateRepository;

import java.util.stream.Collectors;

import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;
import static ru.itterminal.yanmas.commons.model.filter.NumberFilter.TypeComparisonForNumberFilter.*;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {TicketRepositoryTestConfig.class})
@Sql({"/create-ticket-test.sql"})
class NumberFilterSpecificationsFactoryTest {

    public static final String DATE_START = "dateStart";
    @Autowired
    private TicketTemplateRepository repository;

    private final Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "subject"));
    private Page<TicketTemplate> foundTicketTemplate;
    private final TicketTemplateTestHelper ticketTemplateTestHelper = new TicketTemplateTestHelper();
    private final NumberFilterSpecificationsFactory numberFactory =
            new NumberFilterSpecificationsFactory();


    @Test
    void isEmpty_shouldEntityWithDateStartNull_whenComparison_IS_EMPTY() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateStart() == null)
                .collect(Collectors.toList());
        var filter = NumberFilter.builder()
                .typeComparison(IS_EMPTY.toString())
                .build();
        Specification<TicketTemplate> tSpecification =
                numberFactory.makeSpecification(filter, DATE_START);
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void isEmpty_shouldEntityWithDateStartNotNull_whenComparison_IS_NOT_EMPTY() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateStart() != null)
                .collect(Collectors.toList());
        var filter = NumberFilter.builder()
                .typeComparison(IS_NOT_EMPTY.toString())
                .build();
        Specification<TicketTemplate> tSpecification =
                numberFactory.makeSpecification(filter, DATE_START);
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void greatThan_shouldGetEntity_whenComparison_GREATER_THAN_AndDateStart_2020_01_01() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateStart() != null && tt.getDateStart() > TicketTemplateTestHelper.DATE_2020_01_01)
                .collect(Collectors.toList());
        var filter = NumberFilter.builder()
                .typeComparison(GREATER_THAN.toString())
                .valueOne(TicketTemplateTestHelper.DATE_2020_01_01)
                .build();
        Specification<TicketTemplate> tSpecification =
                numberFactory.makeSpecification(filter, DATE_START);
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void greatThanOrEquals_shouldGetEntity_whenComparison_GREATER_THAN_OR_EQUAL_TO_AndDateStart_2020_01_01() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateStart() != null && tt.getDateStart() >= TicketTemplateTestHelper.DATE_2020_01_01)
                .collect(Collectors.toList());
        var filter = NumberFilter.builder()
                .typeComparison(GREATER_THAN_OR_EQUAL_TO.toString())
                .valueOne(TicketTemplateTestHelper.DATE_2020_01_01)
                .build();
        Specification<TicketTemplate> tSpecification =
                numberFactory.makeSpecification(filter, DATE_START);
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void lessThan_shouldGetEntity_whenComparison_LESS_THAN_AndDateStart_2019_01_01() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateStart() != null && tt.getDateStart() < TicketTemplateTestHelper.DATE_2019_01_01)
                .collect(Collectors.toList());
        var filter = NumberFilter.builder()
                .typeComparison(LESS_THAN.toString())
                .valueOne(TicketTemplateTestHelper.DATE_2019_01_01)
                .build();
        Specification<TicketTemplate> tSpecification =
                numberFactory.makeSpecification(filter, DATE_START);
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void lessThanOrEquals_shouldGetEntity_whenComparison_LESS_THAN_OR_EQUAL_TO_AndDateStart_2019_01_01() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateStart() != null && tt.getDateStart() <= TicketTemplateTestHelper.DATE_2019_01_01)
                .collect(Collectors.toList());
        var filter = NumberFilter.builder()
                .typeComparison(LESS_THAN_OR_EQUAL_TO.toString())
                .valueOne(TicketTemplateTestHelper.DATE_2019_01_01)
                .build();
        Specification<TicketTemplate> tSpecification =
                numberFactory.makeSpecification(filter, DATE_START);
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void isBetweenInclusive_shouldGetEntity_whenComparison_IS_BETWEEN_INCLUSIVE_AndDateStart_2019_01_01_AndDateEnd_2020_01_01() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateStart() != null && tt.getDateStart() >= TicketTemplateTestHelper.DATE_2019_01_01 && tt.getDateStart() <= TicketTemplateTestHelper.DATE_2020_01_01)
                .collect(Collectors.toList());
        var filter = NumberFilter.builder()
                .typeComparison(IS_BETWEEN_INCLUSIVE.toString())
                .valueOne(TicketTemplateTestHelper.DATE_2019_01_01)
                .valueTwo(TicketTemplateTestHelper.DATE_2020_01_01)
                .build();
        Specification<TicketTemplate> tSpecification =
                numberFactory.makeSpecification(filter, DATE_START);
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void isBetweenExclusion_shouldGetEntity_whenComparison_IS_BETWEEN_EXCLUSION_AndDate_2019_01_01_And_2020_01_01() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateStart() != null && tt.getDateStart() > TicketTemplateTestHelper.DATE_2019_01_01 && tt.getDateStart() < TicketTemplateTestHelper.DATE_2020_01_01)
                .collect(Collectors.toList());
        var filter = NumberFilter.builder()
                .typeComparison(IS_BETWEEN_EXCLUSION.toString())
                .valueOne(TicketTemplateTestHelper.DATE_2019_01_01)
                .valueTwo(TicketTemplateTestHelper.DATE_2020_01_01)
                .build();
        Specification<TicketTemplate> tSpecification =
                numberFactory.makeSpecification(filter, DATE_START);
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void isNotBetweenInclusive_shouldGetEntity_whenComparison_IS_NOT_BETWEEN_INCLUSIVE_AndDate_2019_01_01_And_2020_01_01() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateStart() != null && (tt.getDateStart() <= TicketTemplateTestHelper.DATE_2019_01_01 || tt.getDateStart() >= TicketTemplateTestHelper.DATE_2020_01_01))
                .collect(Collectors.toList());
        var filter = NumberFilter.builder()
                .typeComparison(IS_NOT_BETWEEN_INCLUSIVE.toString())
                .valueOne(TicketTemplateTestHelper.DATE_2019_01_01)
                .valueTwo(TicketTemplateTestHelper.DATE_2020_01_01)
                .build();
        Specification<TicketTemplate> tSpecification =
                numberFactory.makeSpecification(filter, DATE_START);
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void isNotBetweenExclusion_shouldGetEntity_whenComparison_IS_NOT_BETWEEN_EXCLUSION_AndDate_2019_01_01_And_2020_01_01() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateStart() != null && (tt.getDateStart() < TicketTemplateTestHelper.DATE_2019_01_01 || tt.getDateStart() > TicketTemplateTestHelper.DATE_2020_01_01))
                .collect(Collectors.toList());
        var filter = NumberFilter.builder()
                .typeComparison(IS_NOT_BETWEEN_EXCLUSION.toString())
                .valueOne(TicketTemplateTestHelper.DATE_2019_01_01)
                .valueTwo(TicketTemplateTestHelper.DATE_2020_01_01)
                .build();
        Specification<TicketTemplate> tSpecification =
                numberFactory.makeSpecification(filter, DATE_START);
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void isNotEqualTo_shouldGetEntity_whenComparison_IS_NOT_EQUAL_TO_AndDate_2019_01_01() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateStart() != null && tt.getDateStart() != TicketTemplateTestHelper.DATE_2019_01_01)
                .collect(Collectors.toList());
        var filter = NumberFilter.builder()
                .typeComparison(IS_NOT_EQUAL_TO.toString())
                .valueOne(TicketTemplateTestHelper.DATE_2019_01_01)
                .build();
        Specification<TicketTemplate> tSpecification =
                numberFactory.makeSpecification(filter, DATE_START);
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void isEqualTo_shouldGetEntity_whenComparison_IS_EQUAL_TO_AndDate_2019_01_01() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateStart() != null && tt.getDateStart() == TicketTemplateTestHelper.DATE_2019_01_01)
                .collect(Collectors.toList());
        var filter = NumberFilter.builder()
                .typeComparison(IS_EQUAL_TO.toString())
                .valueOne(TicketTemplateTestHelper.DATE_2019_01_01)
                .build();
        Specification<TicketTemplate> tSpecification =
                numberFactory.makeSpecification(filter, DATE_START);
        foundTicketTemplate = repository.findAll(tSpecification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

}
