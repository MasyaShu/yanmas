package ru.itterminal.botdesk.tickets.model.spec;

import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;
import static ru.itterminal.botdesk.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.CONTAINS_ALL_OF_LIST;
import static ru.itterminal.botdesk.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.CONTAINS_ANY_IN_LIST;
import static ru.itterminal.botdesk.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.IS_EMPTY;
import static ru.itterminal.botdesk.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.IS_EQUAL_TO;
import static ru.itterminal.botdesk.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.IS_NOT_EMPTY;
import static ru.itterminal.botdesk.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.IS_NOT_EQUAL_TO;
import static ru.itterminal.botdesk.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.NOT_CONTAINS_ALL_OF_LIST;
import static ru.itterminal.botdesk.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.NOT_CONTAINS_ANY_IN_LIST;

import java.util.List;
import java.util.UUID;

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

import ru.itterminal.botdesk.commons.model.filter.ListOfBaseEntityFilter;
import ru.itterminal.botdesk.commons.model.spec.ListOfBaseEntityFilterSpecificationsFactory;
import ru.itterminal.botdesk.tickets.model.Ticket;
import ru.itterminal.botdesk.tickets.repository.TicketRepository;
import ru.itterminal.botdesk.tickets.repository.TicketRepositoryTestConfig;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {TicketRepositoryTestConfig.class})
@Sql({"/create-ticket-test.sql"})
class TicketSpecTest {

    public static final String EXECUTORS = "executors";

    @Autowired
    private TicketRepository repository;

    private final Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "subject"));
    private Page<Ticket> foundTickets;

    private final ListOfBaseEntityFilterSpecificationsFactory listFilterFactory =
            new ListOfBaseEntityFilterSpecificationsFactory();

    @Test
    void getTicketByAnyInListOfTicketExecutorsSpec_shouldGetTwoTickets_whenAnyFromPassedIdExistsInListOfExecutors() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(CONTAINS_ANY_IN_LIST.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                                UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(2);
        assertEquals(
                UUID.fromString("b927e5af-0db3-4f55-8da0-a31d0f6421d0"),
                foundTickets.getContent().get(0).getId()
        );
    }

    @Test
    void getTicketByAnyInListOfTicketExecutorsSpec_shouldGetThreeTickets_whenAnyFromPassedIdExistsInListOfExecutors() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(CONTAINS_ANY_IN_LIST.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                                UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de"),
                                UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0"),
                                UUID.randomUUID(),
                                UUID.randomUUID()
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(3);
    }

    @Test
    void getTicketByAnyInListOfTicketExecutorsSpec_shouldGetOneTicket_whenAnyFromPassedIdExistsInListOfExecutors() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(CONTAINS_ANY_IN_LIST.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(1);
        assertEquals(
                UUID.fromString("b927e5af-0db3-4f55-8da0-a31d0f6421d0"),
                foundTickets.getContent().get(0).getId()
        );
    }

    @Test
    void getTicketByAnyInListOfTicketExecutorsSpec_shouldGetZeroTickets_whenPassedRandomId() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(CONTAINS_ANY_IN_LIST.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.randomUUID()
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertThat(foundTickets).isEmpty();

    }

    @Test
    void getTicketByAllInListOfTicketExecutors_shouldGetOneTicket_whenAllPassedIsExistInOneTicket() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(CONTAINS_ALL_OF_LIST.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                                UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(1);
        assertEquals(
                UUID.fromString("b927e5af-0db3-4f55-8da0-a31d0f6421d0"),
                foundTickets.getContent().get(0).getId()
        );
    }

    @Test
    void getTicketByAllInListOfTicketExecutors_shouldGetEmpty_whenThereAreNotExistTicketWithPassedListOfExecutors() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(CONTAINS_ALL_OF_LIST.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                                UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0"),
                                UUID.fromString("d592facb-e6ee-4801-8310-9c7708eb6e6c")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertThat(foundTickets).isEmpty();
    }

    @Test
    void getTicketByAllInListOfTicketExecutors_shouldGetTwoTickets_whenThereAreTwoTicketWithPassedIdInListOfExecutors() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(CONTAINS_ALL_OF_LIST.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(2);
    }

    @Test
    void getTicketByAllInListOfTicketExecutors_shouldGetEmpty_whenPassedIdNotExist() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(CONTAINS_ALL_OF_LIST.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("f7dd8f4c-4d29-4001-a408-e5bf394ac6ff")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertThat(foundTickets).isEmpty();
    }

    @Test
    void getTicketByAllNotInListOfTicketExecutorsSpec_shouldGetFiveTickets_whenAllIdNotExist() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(NOT_CONTAINS_ALL_OF_LIST.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("23b81be8-6954-4b88-b16a-08e894630814"),
                                UUID.fromString("4306c617-ac19-4ea5-9a04-41653eb3d3b7")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(5, foundTickets.getContent().size());
    }

    @Test
    void getTicketByAllNotInListOfTicketExecutorsSpec_shouldGetFourTickets_whenPassedListExistInOneTicket() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(NOT_CONTAINS_ALL_OF_LIST.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                                UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(4, foundTickets.getContent().size());
    }

    @Test
    void getTicketByAllNotInListOfTicketExecutorsSpec_shouldGetFiveTickets_whenSizeOfListEqualThree() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(NOT_CONTAINS_ALL_OF_LIST.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                                UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0"),
                                UUID.fromString("d592facb-e6ee-4801-8310-9c7708eb6e6c")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(5, foundTickets.getContent().size());
    }

    @Test
    void getTicketByAllNotInListOfTicketExecutorsSpec_shouldGetThreeTicket_whenTwoTicketHavePassedIdInListOfExecutors() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(NOT_CONTAINS_ALL_OF_LIST.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(3, foundTickets.getContent().size());
    }

    @Test
    void getTicketByAllNotInListOfTicketExecutorsSpec_shouldGetFiveTickets_whenPassedIdNotExist() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(NOT_CONTAINS_ALL_OF_LIST.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("63eae85b-ac2b-4f6d-9ad5-3aa4f6616f4b")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(5, foundTickets.getContent().size());
    }

    @Test
    void getTicketByNotAnyInListOfTicketExecutorsSpec_shouldGetOne_whenOneTicketDoesntHaveListOfExecutors() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(NOT_CONTAINS_ANY_IN_LIST.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                                UUID.fromString("d592facb-e6ee-4801-8310-9c7708eb6e6c")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(1, foundTickets.getContent().size());
    }

    @Test
    void getTicketByNotAnyInListOfTicketExecutorsSpec_shouldGetThreeTickets_whenTwoFromFiveTicketsHavePassedId() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(NOT_CONTAINS_ANY_IN_LIST.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(3, foundTickets.getContent().size());
    }

    @Test
    void getTicketByNotAnyInListOfTicketExecutorsSpec_shouldGetFiveTickets_whenPassedIdNotExist() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(NOT_CONTAINS_ANY_IN_LIST.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("fd7bd2e2-b078-4c69-8594-904102db49fa")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(5, foundTickets.getContent().size());
    }

    @Test
    void getTicketIsEqualListOfTicketExecutorsSpec_shouldGetOneTicketWithNumber100_whenThereIsEqualListOfExecutors() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(IS_EQUAL_TO.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                                UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(1, foundTickets.getContent().size());
        assertEquals(
                UUID.fromString("b927e5af-0db3-4f55-8da0-a31d0f6421d0"),
                foundTickets.getContent().get(0).getId()
        );
    }

    @Test
    void getTicketIsEqualListOfTicketExecutorsSpec_shouldGetOneTicketWithNumber101_whenThereIsEqualListOfExecutors() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(IS_EQUAL_TO.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("d592facb-e6ee-4801-8310-9c7708eb6e6c"),
                                UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(1, foundTickets.getContent().size());
        assertEquals(
                UUID.fromString("7e1daf3c-d5f5-49a7-86e7-0f295c84941d"),
                foundTickets.getContent().get(0).getId()
        );
    }

    @Test
    void getTicketIsEqualListOfTicketExecutorsSpec_shouldGetEmpty_whenThereAreNotEqualListOfExecutorsWithSizeTwoThree() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(IS_EQUAL_TO.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("d592facb-e6ee-4801-8310-9c7708eb6e6c"),
                                UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de"),
                                UUID.randomUUID()
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(0, foundTickets.getContent().size());
    }

    @Test
    void getTicketIsEqualListOfTicketExecutorsSpec_shouldGetEmpty_whenThereAreNotEqualListOfExecutorsWithSizeTwo() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(IS_EQUAL_TO.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                                UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(0, foundTickets.getContent().size());
    }

    @Test
    void getTicketIsEqualListOfTicketExecutorsSpec_shouldGetEmpty_whenThereAreNotEqualListOfExecutorsWithSizeOne() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(IS_EQUAL_TO.toString())
                .listOfIdEntities(List.of(UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7")))
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(0, foundTickets.getContent().size());
    }

    @Test
    void getTicketIsNotEqualListOfTicketExecutorsSpec_shouldGetFiveTickets_whenPassedOneNotExistId() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(IS_NOT_EQUAL_TO.toString())
                .listOfIdEntities(List.of(UUID.fromString("83776624-be47-480e-9a23-98cab111aa9c")))
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(5, foundTickets.getContent().size());
    }

    @Test
    void getTicketIsNotEqualListOfTicketExecutorsSpec_shouldGetFiveTickets_whenPassedTwoNotExistId() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(IS_NOT_EQUAL_TO.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("83776624-be47-480e-9a23-98cab111aa9c"),
                                UUID.fromString("6642f777-a63b-483f-a977-a4dc7830efc0")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(5, foundTickets.getContent().size());
    }

    @Test
    void getTicketIsNotEqualListOfTicketExecutorsSpec_shouldGetFiveTickets_whenPassedThreeNotExistId() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(IS_NOT_EQUAL_TO.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("83776624-be47-480e-9a23-98cab111aa9c"),
                                UUID.fromString("310d192d-a642-4cb7-abdc-fae63c526281"),
                                UUID.fromString("6642f777-a63b-483f-a977-a4dc7830efc0")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(5, foundTickets.getContent().size());
    }

    @Test
    void getTicketIsNotEqualListOfTicketExecutorsSpec_shouldGetFourTickets_whenPassedTwoExistIdInTheSameTicket() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(IS_NOT_EQUAL_TO.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                                UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(4, foundTickets.getContent().size());
    }

    @Test
    void getTicketIsNotEqualListOfTicketExecutorsSpec_shouldGetFiveTickets_whenPassedTwoExistIdInDifferentTickets() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(IS_NOT_EQUAL_TO.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                                UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(5, foundTickets.getContent().size());
    }

    @Test
    void getTicketIsNotEqualListOfTicketExecutorsSpec_shouldGetFourTickets_whenOneFromFiveTicketsHaveTheSameIdInListOfExecutors() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(IS_NOT_EQUAL_TO.toString())
                .listOfIdEntities(
                        List.of(
                                UUID.fromString("d592facb-e6ee-4801-8310-9c7708eb6e6c")
                        )
                )
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(4, foundTickets.getContent().size());
    }

    @Test
    void getTicketIsEmptyListOfTicketExecutorsSpec_shouldGetOneTicket_whenOnlyOneTicketDoesntHaveExecutors() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(IS_EMPTY.toString())
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(1, foundTickets.getContent().size());
        assertEquals(
                UUID.fromString("a2782c2c-5054-4c12-815e-c26b3c5275ee"),
                foundTickets.getContent().get(0).getId()
        );
    }

    @Test
    void getTicketIsNotEmptyListOfTicketExecutorsSpec_shouldGetFourTickets_whenFourTicketsWithListOfExecutors() {
        var filter = ListOfBaseEntityFilter.builder()
                .typeComparison(IS_NOT_EMPTY.toString())
                .build();
        Specification<Ticket> tSpecification =
                listFilterFactory.makeSpecification(filter, EXECUTORS, Ticket.class);
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(4, foundTickets.getContent().size());
    }

}