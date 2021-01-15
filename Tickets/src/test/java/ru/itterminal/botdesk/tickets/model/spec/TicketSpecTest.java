package ru.itterminal.botdesk.tickets.model.spec;

import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

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

import ru.itterminal.botdesk.tickets.model.Ticket;
import ru.itterminal.botdesk.tickets.repository.TicketRepository;
import ru.itterminal.botdesk.tickets.repository.TicketRepositoryTestConfig;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {TicketRepositoryTestConfig.class, TicketSpec.class})
@Sql({"/create-ticket-test.sql"})
class TicketSpecTest {

    @Autowired
    private TicketRepository repository;

    @Autowired
    private TicketSpec spec;

    private final Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "subject"));
    private Page<Ticket> foundTickets;

    @Test
    void getTicketByAnyInListOfTicketExecutorsSpec_shouldGetTwoTickets() {
        var listTicketExecutorId = List.of(
                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketByAnyInListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(2);
        assertEquals(
                UUID.fromString("b927e5af-0db3-4f55-8da0-a31d0f6421d0"),
                foundTickets.getContent().get(0).getId()
        );
    }

    @Test
    void getTicketByAnyInListOfTicketExecutorsSpec_shouldGetThreeTickets() {
        var listTicketExecutorId = List.of(
                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de"),
                UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0"),
                UUID.randomUUID(),
                UUID.randomUUID()
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketByAnyInListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(3);
    }

    @Test
    void getTicketByAnyInListOfTicketExecutorsSpec_shouldGetOneTicket() {
        var listTicketExecutorId = List.of(UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0"));
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketByAnyInListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(1);
        assertEquals(
                UUID.fromString("b927e5af-0db3-4f55-8da0-a31d0f6421d0"),
                foundTickets.getContent().get(0).getId()
        );
    }

    @Test
    void getTicketByAnyInListOfTicketExecutorsSpec_shouldGetZeroTickets() {
        var listTicketExecutorId = List.of(UUID.randomUUID());
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketByAnyInListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertThat(foundTickets).isEmpty();

    }

    @Test
    void getTicketByAllInListOfTicketExecutors_shouldGetOneTicket() {
        var listTicketExecutorId = List.of(
                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketByAllInListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(1);
        assertEquals(
                UUID.fromString("b927e5af-0db3-4f55-8da0-a31d0f6421d0"),
                foundTickets.getContent().get(0).getId()
        );
    }

    @Test
    void getTicketByAllInListOfTicketExecutors_shouldGetEmpty() {
        var listTicketExecutorId = List.of(
                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0"),
                UUID.fromString("d592facb-e6ee-4801-8310-9c7708eb6e6c")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketByAllInListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertThat(foundTickets).isEmpty();
    }

    @Test
    void getTicketByAllInListOfTicketExecutors_shouldGetTwoTickets() {
        var listTicketExecutorId = List.of(
                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketByAllInListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(2);
    }

    @Test
    void getTicketByAllInListOfTicketExecutors_shouldGetEmpty_whenPassedIdNotExist() {
        var listTicketExecutorId = List.of(
                UUID.fromString("f7dd8f4c-4d29-4001-a408-e5bf394ac6ff")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketByAllInListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertThat(foundTickets).isEmpty();
    }

    @Test
    void getTicketByAllNotInListOfTicketExecutorsSpec_shouldGetFiveTickets_whenAllIdNotExist() {
        var listTicketExecutorId = List.of(
                UUID.fromString("23b81be8-6954-4b88-b16a-08e894630814"),
                UUID.fromString("4306c617-ac19-4ea5-9a04-41653eb3d3b7")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketByAllNotInListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(5,foundTickets.getContent().size());
    }

    @Test
    void getTicketByAllNotInListOfTicketExecutorsSpec_shouldGetFourTickets() {
        var listTicketExecutorId = List.of(
                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketByAllNotInListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(4,foundTickets.getContent().size());
    }

    @Test
    void getTicketByAllNotInListOfTicketExecutorsSpec_shouldGetFiveTickets_whenSizeOfListEqualThree() {
        var listTicketExecutorId = List.of(
                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0"),
                UUID.fromString("d592facb-e6ee-4801-8310-9c7708eb6e6c")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketByAllNotInListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(5,foundTickets.getContent().size());
    }

    @Test
    void getTicketByAllNotInListOfTicketExecutorsSpec_shouldGetThreeTicket_whenAllTicketsHaveSizeOfListExecutorsEqualTwo() {
        var listTicketExecutorId = List.of(
                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketByAllNotInListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(3,foundTickets.getContent().size());
    }

    @Test
    void getTicketByAllNotInListOfTicketExecutorsSpec_shouldGetFiveTickets_whenPassedIdNotExist() {
        var listTicketExecutorId = List.of(
                UUID.fromString("63eae85b-ac2b-4f6d-9ad5-3aa4f6616f4b")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketByAllNotInListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(5,foundTickets.getContent().size());
    }

    @Test
    void getTicketByNotAnyInListOfTicketExecutorsSpec_shouldGetOne_whenOneTicketDoesntHaveListOfExecutors() {
        var listTicketExecutorId = List.of(
                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                UUID.fromString("d592facb-e6ee-4801-8310-9c7708eb6e6c")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketByNotAnyInListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(1,foundTickets.getContent().size());
    }

    @Test
    void getTicketByNotAnyInListOfTicketExecutorsSpec_shouldGetThreeTickets_whenTwoFromFiveTicketsHavePassedId() {
        var listTicketExecutorId = List.of(
                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketByNotAnyInListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(3,foundTickets.getContent().size());
    }

    @Test
    void getTicketByNotAnyInListOfTicketExecutorsSpec_shouldGetFiveTickets_whenPassedIdNotExist() {
        var listTicketExecutorId = List.of(
                UUID.fromString("fd7bd2e2-b078-4c69-8594-904102db49fa")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketByNotAnyInListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(5,foundTickets.getContent().size());
    }

    @Test
    void getTicketIsEqualListOfTicketExecutorsSpec_shouldGetOneTicketWithNumber100() {
        var listTicketExecutorId = List.of(
                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketIsEqualListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(1,foundTickets.getContent().size());
        assertEquals(
                UUID.fromString("b927e5af-0db3-4f55-8da0-a31d0f6421d0"),
                foundTickets.getContent().get(0).getId()
        );
    }

    @Test
    void getTicketIsEqualListOfTicketExecutorsSpec_shouldGetOneTicketWithNumber101() {
        var listTicketExecutorId = List.of(
                UUID.fromString("d592facb-e6ee-4801-8310-9c7708eb6e6c"),
                UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketIsEqualListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(1,foundTickets.getContent().size());
        assertEquals(
                UUID.fromString("7e1daf3c-d5f5-49a7-86e7-0f295c84941d"),
                foundTickets.getContent().get(0).getId()
        );
    }

    @Test
    void getTicketIsEqualListOfTicketExecutorsSpec_shouldGetEmpty_whenSizeListEqualThree() {
        var listTicketExecutorId = List.of(
                UUID.fromString("d592facb-e6ee-4801-8310-9c7708eb6e6c"),
                UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de"),
                UUID.randomUUID()
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketIsEqualListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(0,foundTickets.getContent().size());
    }

    @Test
    void getTicketIsEqualListOfTicketExecutorsSpec_shouldGetEmpty_whenSizeListEqualTwo() {
        var listTicketExecutorId = List.of(
                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketIsEqualListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(0,foundTickets.getContent().size());
    }

    @Test
    void getTicketIsEqualListOfTicketExecutorsSpec_shouldGetEmpty_whenSizeListEqualOne() {
        var listTicketExecutorId = List.of(
                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketIsEqualListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(0,foundTickets.getContent().size());
    }

    @Test
    void getTicketIsNotEqualListOfTicketExecutorsSpec_shouldGetFiveTickets_whenPassedOneNotExistId() {
        var listTicketExecutorId = List.of(
                UUID.fromString("83776624-be47-480e-9a23-98cab111aa9c")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketIsNotEqualListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(5,foundTickets.getContent().size());
    }

    @Test
    void getTicketIsNotEqualListOfTicketExecutorsSpec_shouldGetFiveTickets_whenPassedTwoNotExistId() {
        var listTicketExecutorId = List.of(
                UUID.fromString("83776624-be47-480e-9a23-98cab111aa9c"),
                UUID.fromString("6642f777-a63b-483f-a977-a4dc7830efc0")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketIsNotEqualListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(5,foundTickets.getContent().size());
    }

    @Test
    void getTicketIsNotEqualListOfTicketExecutorsSpec_shouldGetFiveTickets_whenPassedThreeNotExistId() {
        var listTicketExecutorId = List.of(
                UUID.fromString("83776624-be47-480e-9a23-98cab111aa9c"),
                UUID.fromString("310d192d-a642-4cb7-abdc-fae63c526281"),
                UUID.fromString("6642f777-a63b-483f-a977-a4dc7830efc0")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketIsNotEqualListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(5,foundTickets.getContent().size());
    }

    @Test
    void getTicketIsNotEqualListOfTicketExecutorsSpec_shouldGetFourTickets_whenPassedTwoExistIdInTheSameTicket() {
        var listTicketExecutorId = List.of(
                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketIsNotEqualListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(4,foundTickets.getContent().size());
    }

    @Test
    void getTicketIsNotEqualListOfTicketExecutorsSpec_shouldGetFiveTickets_whenPassedTwoExistIdInDifferentTickets() {
        var listTicketExecutorId = List.of(
                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketIsNotEqualListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(5,foundTickets.getContent().size());
    }

    @Test
    void getTicketIsNotEqualListOfTicketExecutorsSpec_shouldGetFourTickets_whenOneFromFiveTicketsHaveTheSameIdInListOfExecutors() {
        var listTicketExecutorId = List.of(
                UUID.fromString("d592facb-e6ee-4801-8310-9c7708eb6e6c")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketIsNotEqualListOfTicketExecutorsSpec(listTicketExecutorId));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(4,foundTickets.getContent().size());
    }

    @Test
    void getTicketIsEmptyListOfTicketExecutorsSpec_shouldGetOneTicket_whenOnlyOneTicketDoesntHaveExecutors() {
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketIsEmptyListOfTicketExecutorsSpec());
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(1,foundTickets.getContent().size());
        assertEquals(
                UUID.fromString("a2782c2c-5054-4c12-815e-c26b3c5275ee"),
                foundTickets.getContent().get(0).getId()
        );
    }

    @Test
    void getTicketIsNotEmptyListOfTicketExecutorsSpec_shouldGetFourTickets_whenFourTicketsWithListOfExecutors() {
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketIsNotEmptyListOfTicketExecutorsSpec());
        foundTickets = repository.findAll(tSpecification, pageable);
        assertEquals(4,foundTickets.getContent().size());
    }

    @Test
    void multipleSpec_shouldGetFiveTickets_whenAllTicketsHaveAlmostEqualFields() {
        var listTicketExecutorId = List.of(
                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0")
        );
        Specification<Ticket> tSpecification = Specification
                .where(spec.getTicketByAllInListOfTicketExecutorsSpec(listTicketExecutorId))
                .or(spec.getTicketBySubjectSpec("subject ticket number"))
                .or(spec.getTicketByDescriptionSpec("description ticket number"))
                .or(spec.getTicketByListOfAuthorsSpec(List.of(UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de"))));
        foundTickets = repository.findAll(tSpecification, pageable);
        assertThat(foundTickets).isNotNull().hasSize(5);
    }

}