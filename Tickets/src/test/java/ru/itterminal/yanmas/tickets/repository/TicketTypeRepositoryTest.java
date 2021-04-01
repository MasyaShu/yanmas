package ru.itterminal.yanmas.tickets.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

import java.util.UUID;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import ru.itterminal.yanmas.tickets.model.test.TicketTypeTestHelper;

@SuppressWarnings("OptionalGetWithoutIsPresent")
@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {TicketRepositoryTestConfig.class, TicketTypeRepository.class})
@Sql({"/create-ticket-test.sql"})
class TicketTypeRepositoryTest {

    @Autowired
    private TicketTypeRepository ticketTypeRepository;

    @PersistenceContext
    EntityManager entityManager;

    private final TicketTypeTestHelper helper = new TicketTypeTestHelper();

    private static final UUID ACCOUNT_1_ID = UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de");
    private static final UUID NOT_EXIST_ID = UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e8");
    private static final UUID EXIST_ID = UUID.fromString("7f66b241-f8ec-4912-8f58-a4ceef2dd4c9");
    private static final String NOT_EXIST_NAME = "NotExistName";
    private static final String EXIST_NAME = "ticketTypes1";
    private static final UUID ACCOUNT_ID_NOT_EXIST = UUID.fromString("3ee38c7d-df86-40a7-8805-f75649f15cd1");

    @Test
    void getByNameAndAccount_IdAndIdNot_shouldGetEmptyList_whenNameNotExistAndIdNotExistInDatabase() {
        assertTrue(
                ticketTypeRepository.getByNameAndAccount_IdAndIdNot(NOT_EXIST_NAME, ACCOUNT_1_ID, NOT_EXIST_ID)
                        .isEmpty());
    }

    @Test
    void getByNameAndAccount_IdAndIdNot_shouldGetEmptyList_whenNameNotExistAndIdExistInDatabase() {
        assertTrue(
                ticketTypeRepository.getByNameAndAccount_IdAndIdNot(NOT_EXIST_NAME, ACCOUNT_1_ID, EXIST_ID).isEmpty());
    }

    @Test
    void getByNameAndAccount_IdAndIdNot_shouldGetNotNull_whenNameExistAndIdNotExistInDatabase() {
        String name = ticketTypeRepository
                .getByNameAndAccount_IdAndIdNot(EXIST_NAME, ACCOUNT_1_ID, NOT_EXIST_ID).get(0).getName();
        assertEquals(EXIST_NAME, name);
    }

    @Test
    void getByNameAndAccount_IdAndIdNot_shouldGetEmptyList_whenNameExistAndIdExistInDatabase() {
        assertTrue(ticketTypeRepository.getByNameAndAccount_IdAndIdNot(EXIST_NAME, ACCOUNT_1_ID, EXIST_ID).isEmpty());
    }

    @Test
    void getByNameAndAccount_Id_shouldGetEmptyList_whenNameNotExistAndAccountIdExist() {
        assertTrue(ticketTypeRepository.getByNameAndAccount_Id(NOT_EXIST_NAME, ACCOUNT_1_ID).isEmpty());
    }

    @Test
    void getByNameAndAccount_Id_shouldGetNotNull_whenNameExistAndAccountIdExist() {
        assertEquals(EXIST_NAME, ticketTypeRepository.getByNameAndAccount_Id(EXIST_NAME, ACCOUNT_1_ID).get().getName());
    }

    @Test
    void getByNameAndAccount_Id_shouldGetEmptyList_whenNameExistAndAccountIdNotExist() {
        assertTrue(ticketTypeRepository.getByNameAndAccount_Id(NOT_EXIST_NAME, ACCOUNT_ID_NOT_EXIST).isEmpty());
    }

    @Test
    void getByIdAndAccount_Id_shouldGetEmptyList_whenGroupIdNotExistAndAccountIdExist() {
        assertTrue(ticketTypeRepository.getByIdAndAccount_Id(NOT_EXIST_ID, ACCOUNT_1_ID).isEmpty());
    }

    @Test
    void getByIdAndAccount_Id_shouldGetNotNull_whenGroupIdExistAndAccountIdExist() {
        assertEquals(EXIST_NAME, ticketTypeRepository.getByIdAndAccount_Id(EXIST_ID, ACCOUNT_1_ID).get().getName());
    }

    @Test
    void getByIdAndAccount_Id_shouldGetEmptyList_whenGroupIdExistAndAccountIdNotExist() {
        assertTrue(ticketTypeRepository.getByIdAndAccount_Id(NOT_EXIST_ID, ACCOUNT_ID_NOT_EXIST).isEmpty());
    }

    @Test
    void update_shouldNotSaveNewValue_whenColumnAsUpdatableIsFalse() {
        var ticketTypeId = helper.setPredefinedValidEntityList().get(0).getId();
        var ticketType = ticketTypeRepository.findById(ticketTypeId).get();
        assertEquals(true, ticketType.getIsPredefinedForNewTicket());
        ticketType.setIsPredefinedForNewTicket(false);
        ticketTypeRepository.update(ticketType);
        entityManager.clear();
        ticketType =  ticketTypeRepository.findById(ticketTypeId).get();
        assertEquals(true, ticketType.getIsPredefinedForNewTicket());
    }

}