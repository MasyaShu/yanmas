package ru.itterminal.botdesk.aau.repository;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = UserRepositoryTestConfig.class)
@Sql({"/create-user-test.sql"})
class UserRepositoryTest {

    @Autowired
    private UserRepository repository;

    private static final String EXIST_EMAIL = "m@m.ru";
    private static final String NOT_EXIST_EMAIL = "n@n.ru";
    private static final UUID EXIST_ID = UUID.fromString("d592facb-e6ee-4801-8310-9c7708eb6e6c");
    private static final UUID NOT_EXIST_ID = UUID.fromString("cb9e8816-7bed-4bb6-b3ea-3aa0eee247b6");

    @Test
    public void getByEmailAndIdNot_shouldGetNull_whenEmailNotExistAndIdNotExistInDatabase() {
        assertNull(repository.getByEmailAndIdNot(NOT_EXIST_EMAIL, NOT_EXIST_ID));
    }

    @Test
    public void getByEmailAndIdNot_shouldGetNull_whenEmailNotExistAndIdExistInDatabase() {
        assertNull(repository.getByEmailAndIdNot(NOT_EXIST_EMAIL, EXIST_ID));
    }

    @Test
    public void getByEmailAndIdNot_shouldGetNotNull_whenEmailExistAndIdNotExistInDatabase() {
        assertTrue(repository.getByEmailAndIdNot(EXIST_EMAIL, NOT_EXIST_ID).getEmail().equals(EXIST_EMAIL));
    }

    @Test
    public void getByEmailAndIdNot_shouldGetNull_whenEmailExistAndIdExistInDatabase() {
        assertNull(repository.getByEmailAndIdNot(EXIST_EMAIL, EXIST_ID));
    }
}