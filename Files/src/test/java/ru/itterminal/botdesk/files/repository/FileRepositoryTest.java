package ru.itterminal.botdesk.files.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

import java.util.List;
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

    private final UUID UUID_ENTITY = UUID.fromString("c2760b6b-33a4-4ee0-bfad-f81128c08754");
    private final UUID UUID_ACCOUNT = UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de");

    @Autowired
    FileRepository repository;

    @Test
    void findAllByEntityId_shouldFindTwoFiles_whenTwoFilesExistInDatabaseForPassedEntityId() {
        List<File> fileList = repository.findAllByEntityIdAndAccount_Id(UUID_ENTITY, UUID_ACCOUNT);
        assertEquals(2, fileList.size());
    }
}