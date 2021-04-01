package ru.itterminal.yanmas.tickets.model.spec;

import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;
import static ru.itterminal.yanmas.commons.model.filter.StringFilter.TypeComparisonForStringFilter.IS_EMPTY;
import static ru.itterminal.yanmas.commons.model.filter.StringFilter.TypeComparisonForStringFilter.IS_NOT_EMPTY;
import static ru.itterminal.yanmas.commons.model.filter.StringFilter.TypeComparisonForStringFilter.TEXT_CONTAINS;
import static ru.itterminal.yanmas.commons.model.filter.StringFilter.TypeComparisonForStringFilter.TEXT_ENDS_WITH;
import static ru.itterminal.yanmas.commons.model.filter.StringFilter.TypeComparisonForStringFilter.TEXT_EQUALS;
import static ru.itterminal.yanmas.commons.model.filter.StringFilter.TypeComparisonForStringFilter.TEXT_NOT_CONTAINS;
import static ru.itterminal.yanmas.commons.model.filter.StringFilter.TypeComparisonForStringFilter.TEXT_STARTS_WITH;

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

import ru.itterminal.yanmas.commons.model.filter.StringFilter;
import ru.itterminal.yanmas.commons.model.spec.StringFilterSpecificationsFactory;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.repository.TicketRepository;
import ru.itterminal.yanmas.tickets.repository.TicketRepositoryTestConfig;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {TicketRepositoryTestConfig.class})
@Sql({"/create-ticket-test.sql"})
class StringFilterSpecificationsFactoryTest {

    public static final String SUBJECT = "subject";
    public static final String DESCRIPTION = "description";

    @Autowired
    private TicketRepository repository;

    private final Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "subject"));
    private Page<Ticket> foundTickets;

    private final StringFilterSpecificationsFactory stringFilterFactory =
            new StringFilterSpecificationsFactory();

    private final StringFilter filter = StringFilter.builder().build();

    @Test
    void IS_EMPTY_shouldGetTwoTickets_whenTwoTicketsHaveNullOrEmptySubject() {
        filter.setTypeComparison(IS_EMPTY.toString());
        Specification<Ticket> specification =
                stringFilterFactory.makeSpecification(filter, SUBJECT);
        foundTickets = repository.findAll(specification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(2);
    }

    @Test
    void IS_NOT_EMPTY_shouldGetThreeTickets_whenThreeTicketsHaveNotNullOrEmptySubject() {
        filter.setTypeComparison(IS_NOT_EMPTY.toString());
        Specification<Ticket> specification =
                stringFilterFactory.makeSpecification(filter, SUBJECT);
        foundTickets = repository.findAll(specification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(3);
    }

    @Test
    void TEXT_CONTAINS_shouldGetThreeTickets_whenThreeTicketsContainsPassedValue() {
        filter.setTypeComparison(TEXT_CONTAINS.toString());
        filter.setValue("subject");
        Specification<Ticket> specification =
                stringFilterFactory.makeSpecification(filter, SUBJECT);
        foundTickets = repository.findAll(specification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(3);
    }

    @Test
    void TEXT_CONTAINS_shouldGetFiveTickets_whenAllTicketsHavePassedValue() {
        filter.setTypeComparison(TEXT_CONTAINS.toString());
        filter.setValue("description");
        Specification<Ticket> specification =
                stringFilterFactory.makeSpecification(filter, DESCRIPTION);
        foundTickets = repository.findAll(specification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(5);
    }

    @Test
    void TEXT_NOT_CONTAINS_shouldGetTwoTickets_whenOneTicketNotContainsPassedValueAndOneTicketHasValueEqualNull() {
        filter.setTypeComparison(TEXT_NOT_CONTAINS.toString());
        filter.setValue("subject");
        Specification<Ticket> specification =
                stringFilterFactory.makeSpecification(filter, SUBJECT);
        foundTickets = repository.findAll(specification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(1);
    }

    @Test
    void TEXT_NOT_CONTAINS_shouldGetZeroTickets_whenAllTicketsHavePassedValue() {
        filter.setTypeComparison(TEXT_NOT_CONTAINS.toString());
        filter.setValue("description");
        Specification<Ticket> specification =
                stringFilterFactory.makeSpecification(filter, DESCRIPTION);
        foundTickets = repository.findAll(specification, pageable);
        assertThat(foundTickets).isEmpty();
    }

    @Test
    void TEXT_STARTS_WITH_shouldGetFiveTickets_whenAllTicketsHaveValueThatStartsWithPassedValue() {
        filter.setTypeComparison(TEXT_STARTS_WITH.toString());
        filter.setValue("description");
        Specification<Ticket> specification =
                stringFilterFactory.makeSpecification(filter, DESCRIPTION);
        foundTickets = repository.findAll(specification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(5);
    }

    @Test
    void TEXT_STARTS_WITH_shouldGetZeroTickets_whenAllTicketsHaveNotValueThatStartsWithPassedValue() {
        filter.setTypeComparison(TEXT_STARTS_WITH.toString());
        filter.setValue("123");
        Specification<Ticket> specification =
                stringFilterFactory.makeSpecification(filter, DESCRIPTION);
        foundTickets = repository.findAll(specification, pageable);
        assertThat(foundTickets).isEmpty();
    }

    @Test
    void TEXT_STARTS_WITH_shouldGetThreeTickets_whenThreeTicketsHaveValueThatStartsWithPassedValue() {
        filter.setTypeComparison(TEXT_STARTS_WITH.toString());
        filter.setValue("subject");
        Specification<Ticket> specification =
                stringFilterFactory.makeSpecification(filter, SUBJECT);
        foundTickets = repository.findAll(specification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(3);
    }

    @Test
    void TEXT_ENDS_WITH_shouldGetOneTickets_whenOneTicketHaveValueThatEndsWithPassedValue() {
        filter.setTypeComparison(TEXT_ENDS_WITH.toString());
        filter.setValue("100");
        Specification<Ticket> specification =
                stringFilterFactory.makeSpecification(filter, SUBJECT);
        foundTickets = repository.findAll(specification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(1);
    }

    @Test
    void TEXT_ENDS_WITH_shouldGetZeroTickets_whenNoOneTicketHaveValueThatEndsWithPassedValue() {
        filter.setTypeComparison(TEXT_ENDS_WITH.toString());
        filter.setValue("999");
        Specification<Ticket> specification =
                stringFilterFactory.makeSpecification(filter, SUBJECT);
        foundTickets = repository.findAll(specification, pageable);
        assertThat(foundTickets).isEmpty();
    }

    @Test
    void TEXT_EQUALS_shouldGetOneTicket_whenOneTicketHasValueThatEqualWithPassedValue() {
        filter.setTypeComparison(TEXT_EQUALS.toString());
        filter.setValue("subject ticket number 100");
        Specification<Ticket> specification =
                stringFilterFactory.makeSpecification(filter, SUBJECT);
        foundTickets = repository.findAll(specification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(1);
    }

    @Test
    void TEXT_EQUALS_shouldGetZeroTickets_whenNoOneTicketsHaveValueThatEqualWithPassedValue() {
        filter.setTypeComparison(TEXT_EQUALS.toString());
        filter.setValue("999");
        Specification<Ticket> specification =
                stringFilterFactory.makeSpecification(filter, SUBJECT);
        foundTickets = repository.findAll(specification, pageable);
        assertThat(foundTickets).isEmpty();
    }



}