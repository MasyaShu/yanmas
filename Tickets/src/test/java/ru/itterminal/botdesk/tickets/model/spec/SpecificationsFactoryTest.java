package ru.itterminal.botdesk.tickets.model.spec;

import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;
import static ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.NOT_EXIST_IN;
import static ru.itterminal.botdesk.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.CONTAINS_ANY_IN_LIST;
import static ru.itterminal.botdesk.commons.model.filter.NumberFilter.TypeComparisonForNumberFilter.IS_NOT_EMPTY;
import static ru.itterminal.botdesk.commons.model.filter.StringFilter.TypeComparisonForStringFilter.IS_EMPTY;
import static ru.itterminal.botdesk.tickets.model.test.TicketTemplateTestHelper.TICKET_TYPE_ID_1;
import static ru.itterminal.botdesk.tickets.model.test.TicketTemplateTestHelper.TICKET_TYPE_ID_2;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

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
import ru.itterminal.botdesk.commons.model.filter.BooleanFilter;
import ru.itterminal.botdesk.commons.model.filter.ListOfBaseEntityFilter;
import ru.itterminal.botdesk.commons.model.filter.NumberFilter;
import ru.itterminal.botdesk.commons.model.filter.StringFilter;
import ru.itterminal.botdesk.commons.model.spec.SpecificationsFactory;
import ru.itterminal.botdesk.tickets.model.Ticket;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;
import ru.itterminal.botdesk.tickets.model.test.TicketTemplateTestHelper;
import ru.itterminal.botdesk.tickets.repository.TicketRepository;
import ru.itterminal.botdesk.tickets.repository.TicketRepositoryTestConfig;
import ru.itterminal.botdesk.tickets.repository.TicketTemplateRepository;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {TicketRepositoryTestConfig.class,TicketRepositoryTestConfig.class,
        SpecificationsFactory.class})
@Sql({"/create-ticket-test.sql"})
class SpecificationsFactoryTest {

    public static final String SUBJECT = "subject";
    public static final String IS_FINISHED = "isFinished";
    public static final String EXECUTORS = "executors";
    public static final String DATE_START = "dateStart";
    private static final String TICKET_TYPE = "ticketType";

    @Autowired
    private TicketRepository ticketRepository;

    @Autowired
    private TicketTemplateRepository ticketTemplateRepository;

    @Autowired
    private SpecificationsFactory specificationsFactory;

    private final Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "subject"));
    private Page<Ticket> foundTickets;
    private Page<TicketTemplate> foundTicketTemplate;
    private final TicketTemplateTestHelper ticketTemplateTestHelper = new TicketTemplateTestHelper();

    private final StringFilter stringFilter = StringFilter.builder().build();
    private final BooleanFilter booleanFilter = BooleanFilter.builder().build();
    private final NumberFilter numberFilter = NumberFilter.builder().build();
    private final BaseEntityFilter baseEntityFilter = BaseEntityFilter.builder().build();
    private final ListOfBaseEntityFilter listOfBaseEntityFilter = ListOfBaseEntityFilter.builder().build();

    @Test
    void stringFilter_shouldGetTwoTickets_whenTwoTicketsHaveNullOrEmptySubject() {
        stringFilter.setTypeComparison(IS_EMPTY.toString());
        Specification<Ticket> specification = Specification
                .where(specificationsFactory.makeSpecification(Ticket.class, SUBJECT, stringFilter));
        foundTickets = ticketRepository.findAll(specification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(2);
    }

    @Test
    void booleanFilter_shouldGetFourTicket_whenFourTicketHasPassedValue() {
        booleanFilter.setValue(false);
        Specification<Ticket> specification = Specification
                .where(specificationsFactory.makeSpecification(Ticket.class, IS_FINISHED, booleanFilter));
        foundTickets = ticketRepository.findAll(specification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(4);
    }

    @Test
    void listOfBaseEntityFilter_shouldGetTwoTickets_whenAnyFromPassedIdExistsInListOfExecutors() {
        listOfBaseEntityFilter.setTypeComparison(CONTAINS_ANY_IN_LIST.toString());
        listOfBaseEntityFilter.setListOfIdEntities(
                List.of(
                        UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                        UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0")
                )
        );
        Specification<Ticket> specification = Specification
                .where(specificationsFactory.makeSpecification(Ticket.class, EXECUTORS, listOfBaseEntityFilter));
        foundTickets = ticketRepository.findAll(specification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(2);
    }

    @Test
    void numberFilter_shouldEntityWithDateStartNotNull_whenComparison_IS_NOT_EMPTY() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt ->  tt.getDateStart() != null)
                .collect(Collectors.toList());
        numberFilter.setTypeComparison(IS_NOT_EMPTY.toString());
        Specification<TicketTemplate> specification = Specification
                .where(specificationsFactory.makeSpecification(TicketTemplate.class, DATE_START, numberFilter));
        foundTicketTemplate = ticketTemplateRepository.findAll(specification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void baseEntityFilter_shouldGetEntity_whenComparison_NOT_EXIST_IN() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt -> tt.getTicketType() != null
                        && !tt.getTicketType().getId().equals(UUID.fromString(TICKET_TYPE_ID_1))
                        && !tt.getTicketType().getId().equals(UUID.fromString(TICKET_TYPE_ID_2)))
                .collect(Collectors.toList());
        baseEntityFilter.setTypeComparison(NOT_EXIST_IN.toString());
        baseEntityFilter.setListOfIdEntities(List.of(UUID.fromString(TICKET_TYPE_ID_1),
                                                     UUID.fromString(TICKET_TYPE_ID_2)));
        Specification<TicketTemplate> specification = Specification
                .where(specificationsFactory.makeSpecification(TicketTemplate.class, TICKET_TYPE, baseEntityFilter));
        foundTicketTemplate = ticketTemplateRepository.findAll(specification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }


}