package ru.itterminal.botdesk.files.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
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
import org.springframework.transaction.annotation.Transactional;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.test.AccountTestHelper;
import ru.itterminal.botdesk.files.model.File;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {FileRepositoryTestConfig.class})
@Sql({"/create-files-test.sql"})
class FileRepositoryTest {

    public static final String FILE_NAME = "File name";
    public static final int SIZE = 100;
    private final UUID FILE_ID = UUID.fromString("d286a51a-a834-48ed-a39b-5ae3d0640001");
    private final UUID ACCOUNT_ID = UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de");
    private final UUID AUTHOR_ID = UUID.fromString("ce493fd4-20ca-4d93-85ce-9da4254f311b");

    @Autowired
    FileRepository repository;

    @PersistenceContext
    EntityManager entityManager;

    @Test
    void findAllByEntityId_shouldFindTwoFiles_whenTwoFilesExistInDatabaseForPassedEntityId() {
        File file = repository.findByAccountIdAndAuthorIdAndId(ACCOUNT_ID, AUTHOR_ID, FILE_ID).get();
        assertNotNull(file);
    }

    @Test
    @Transactional
    void update_shouldNotUpdateCreatedAt_whenUpdateEntity () {
        File file = repository.findByAccountIdAndAuthorIdAndId(ACCOUNT_ID, AUTHOR_ID, FILE_ID).get();
        var createdAtFromDatabaseBeforeUpdate = file.getCreatedAt();
        file.setCreatedAt(null);
        repository.update(file);
        entityManager.clear();
        file = repository.findByAccountIdAndAuthorIdAndId(ACCOUNT_ID, AUTHOR_ID, FILE_ID).get();
        assertEquals(createdAtFromDatabaseBeforeUpdate, file.getCreatedAt());
    }

    @Test
    void save_shouldAutoSetValueForCreatedAt_whenCreateEntity () {
        AccountTestHelper accountTestHelper = new AccountTestHelper();
        Account account = accountTestHelper.getRandomValidEntity();
        account.setId(ACCOUNT_ID);
        File file = File.builder()
                .id(UUID.randomUUID())
                .deleted(false)
                .fileName(FILE_NAME)
                .size(SIZE)
                .account(account)
                .entityId(UUID.randomUUID())
                .build();
        File savedFile = repository.save(file);
        assertNotNull(savedFile.getCreatedAt());
    }

}