package ru.itterminal.yanmas.tickets.model.spec;

import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

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

import ru.itterminal.yanmas.commons.model.filter.BooleanFilter;
import ru.itterminal.yanmas.commons.model.spec.BooleanFilterSpecificationsFactory;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.repository.TicketRepository;
import ru.itterminal.yanmas.tickets.repository.TicketRepositoryTestConfig;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {TicketRepositoryTestConfig.class})
@Sql({"/create-ticket-test.sql"})
class BooleanFilterSpecificationsFactoryTest {

    public static final String IS_FINISHED = "isFinished";
    @Autowired
    private TicketRepository repository;

    private final Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "subject"));
    private Page<Ticket> foundTickets;

    private final BooleanFilterSpecificationsFactory booleanFilterFactory =
            new BooleanFilterSpecificationsFactory();

    private final BooleanFilter filter = BooleanFilter.builder().build();

    @Test
    void makeSpecification_shouldGetOneTicket_whenOneTicketHasPassedValue() {
        filter.setValue(true);
        Specification<Ticket> specification =
                booleanFilterFactory.makeSpecification(filter, IS_FINISHED);
        foundTickets = repository.findAll(specification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(1);
    }

    @Test
    void makeSpecification_shouldGetFourTicket_whenFourTicketHasPassedValue() {
        filter.setValue(false);
        Specification<Ticket> specification =
                booleanFilterFactory.makeSpecification(filter, IS_FINISHED);
        foundTickets = repository.findAll(specification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(4);
    }


}