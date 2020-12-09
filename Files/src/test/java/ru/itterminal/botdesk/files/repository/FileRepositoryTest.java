package ru.itterminal.botdesk.files.repository;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import ru.itterminal.botdesk.files.model.File;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {FileRepositoryTestConfig.class})
@Sql({"/create-files-test.sql"})
class FileRepositoryTest {

    private final UUID UUID_FILE = UUID.fromString("d286a51a-a834-48ed-a39b-5ae3d0640001");
    private final UUID UUID_ACCOUNT = UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de");

    @Autowired
    FileRepository repository;

    @Test
    void findAllByEntityId_shouldFindTwoFiles_whenTwoFilesExistInDatabaseForPassedEntityId() {
        File file = repository.findByAccountIdAndId(UUID_ACCOUNT, UUID_FILE).get();
        assertNotNull(file);
    }
}