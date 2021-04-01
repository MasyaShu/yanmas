package ru.itterminal.yanmas.tickets.model.spec;

import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;
import static ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.NOT_EXIST_IN;
import static ru.itterminal.yanmas.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.CONTAINS_ANY_IN_LIST;
import static ru.itterminal.yanmas.commons.model.filter.NumberFilter.TypeComparisonForNumberFilter.IS_NOT_EMPTY;

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
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import ru.itterminal.yanmas.aau.model.test.AccountTestHelper;
import ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter;
import ru.itterminal.yanmas.commons.model.filter.BooleanFilter;
import ru.itterminal.yanmas.commons.model.filter.ListOfBaseEntityFilter;
import ru.itterminal.yanmas.commons.model.filter.NumberFilter;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.model.TicketTemplate;
import ru.itterminal.yanmas.tickets.model.dto.TicketFilterDto;
import ru.itterminal.yanmas.tickets.model.dto.TicketTemplateFilterDto;
import ru.itterminal.yanmas.tickets.model.test.TicketTemplateTestHelper;
import ru.itterminal.yanmas.tickets.repository.TicketRepository;
import ru.itterminal.yanmas.tickets.repository.TicketRepositoryTestConfig;
import ru.itterminal.yanmas.tickets.repository.TicketTemplateRepository;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {TicketRepositoryTestConfig.class, SpecificationsFactory.class})
@Sql({"/create-ticket-test.sql"})
class SpecificationsFactoryTest {

    @Autowired
    private TicketTemplateRepository ticketTemplateRepository;

    @Autowired
    private TicketRepository ticketRepository;

    @Autowired
    private SpecificationsFactory specificationsFactory;

    private final Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "subject"));
    private Page<Ticket> foundTickets;
    private Page<TicketTemplate> foundTicketTemplate;
    private final TicketTemplateTestHelper ticketTemplateTestHelper = new TicketTemplateTestHelper();

    private final AccountTestHelper accountTestHelper = new AccountTestHelper();
    private final UUID ACCOUNT_1_ID = accountTestHelper.getPredefinedValidEntityList().get(0).getId();

    @Test
    void booleanFilter_shouldGetFourTicket_whenFourTicketHasPassedValueEqualFalse() {
        var isFinishedFilter = BooleanFilter.builder()
                .value(false)
                .build();
        var filterDto = TicketFilterDto.builder()
                .isFinished(isFinishedFilter)
                .sortByFields(List.of("deleted"))
                .build();
        var specification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(Ticket.class, filterDto, ACCOUNT_1_ID);
        foundTickets = ticketRepository.findAll(specification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(4);
    }

    @Test
    void listOfBaseEntityFilter_shouldGetTwoTickets_whenAnyFromPassedIdExistsInListOfExecutors() {
        var executorsFilter = ListOfBaseEntityFilter.builder()
                .typeComparison(CONTAINS_ANY_IN_LIST.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                                UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0")
                        )
                )
                .build();
        var filterDto = TicketFilterDto.builder()
                .executors(executorsFilter)
                .sortByFields(List.of("deleted"))
                .build();
        var specification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(Ticket.class, filterDto, ACCOUNT_1_ID);
        foundTickets = ticketRepository.findAll(specification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(2);
    }

    @Test
    void numberFilter_shouldGetThreeEntityWithDateStartNotNull_whenComparison_IS_NOT_EMPTY() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt -> tt.getDateStart() != null && tt.getAccount().getId().equals(ACCOUNT_1_ID))
                .collect(Collectors.toList());
        var dateStartFilter = NumberFilter.builder()
                .typeComparison(IS_NOT_EMPTY.toString())
                .build();
        var filterDto = TicketTemplateFilterDto.builder()
                .dateStart(dateStartFilter)
                .sortByFields(List.of("deleted"))
                .build();
        var specification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(TicketTemplate.class, filterDto, ACCOUNT_1_ID);
        foundTicketTemplate = ticketTemplateRepository.findAll(specification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

    @Test
    void baseEntityFilter_shouldGetEntity_whenComparison_NOT_EXIST_IN() {
        var ticketTemplates = ticketTemplateTestHelper.setPredefinedValidEntityList();
        var expectedTicketTemplates = ticketTemplates.stream()
                .filter(tt -> tt.getTicketType() != null
                        && !tt.getTicketType().getId().equals(UUID.fromString(TicketTemplateTestHelper.TICKET_TYPE_ID_1))
                        && !tt.getTicketType().getId().equals(UUID.fromString(TicketTemplateTestHelper.TICKET_TYPE_ID_2))
                        && tt.getAccount().getId().equals(ACCOUNT_1_ID)
                )
                .collect(Collectors.toList());
        var ticketType = BaseEntityFilter.builder()
                .typeComparison(NOT_EXIST_IN.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString(TicketTemplateTestHelper.TICKET_TYPE_ID_1),
                                UUID.fromString(TicketTemplateTestHelper.TICKET_TYPE_ID_2)
                        )
                )
                .build();
        var filterDto = TicketTemplateFilterDto.builder()
                .ticketType(ticketType)
                .sortByFields(List.of("deleted"))
                .build();
        var specification = specificationsFactory
                .makeSpecificationFromEntityFilterDto(TicketTemplate.class, filterDto, ACCOUNT_1_ID);
        foundTicketTemplate = ticketTemplateRepository.findAll(specification, pageable);
        Assertions.assertThat(foundTicketTemplate).isNotNull().hasSize(expectedTicketTemplates.size());
        assertThat(foundTicketTemplate.getContent()).containsExactlyInAnyOrderElementsOf(expectedTicketTemplates);
    }

}