package ru.itterminal.botdesk.files.repository;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Random;
import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(classes = {FileSystemRepository.class})
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class FileSystemRepositoryTest {

    @Autowired
    private FileSystemRepository fileSystemRepository;

    private byte[] data;

    @Value("${dir.uploaded.files}")
    private String dirUploadedFiles;

    private final UUID accountId = UUID.randomUUID();
    private final UUID fileId = UUID.randomUUID();
    private final UUID emptyFileId = UUID.randomUUID();

    @BeforeAll
    void setUp() {
        data = new byte[10];
        new Random().nextBytes(data);
    }

    @Test
    @Order(10)
    void save_shouldSaveFile_whenPassedValidData() throws IOException {
        Files.createDirectories(
                Paths.get(
                        dirUploadedFiles,
                        accountId.toString()
                )
        );
        fileSystemRepository.save(
                data,
                Paths.get(
                        dirUploadedFiles,
                        accountId.toString(),
                        fileId.toString()
                )
        );
    }

    @Test
    @Order(20)
    void save_shouldSaveFile_whenSizeOfFileIsZero() throws IOException {
        fileSystemRepository.save(
                new byte[0],
                Paths.get(
                        dirUploadedFiles,
                        accountId.toString(),
                        emptyFileId.toString()
                )
        );
    }

    @Test
    @Order(30)
    void findInFileSystem_shouldGetFileSystemResource_whenPassedValidParameters() throws IOException {
        var location = Paths.get(
                dirUploadedFiles,
                accountId.toString(),
                fileId.toString()
        );
        var fileSystemResource = fileSystemRepository.findInFileSystem(location);
        var dataFromFile = fileSystemResource.getInputStream().readAllBytes();
        assertArrayEquals(data, dataFromFile);
    }

    @Test
    @Order(40)
    void findInFileSystem_shouldGetEmpty_whenFileIsEmpty() throws IOException {
        var location = Paths.get(
                dirUploadedFiles,
                accountId.toString(),
                emptyFileId.toString()
        );
        var fileSystemResource = fileSystemRepository.findInFileSystem(location);
        var dataFromFile = fileSystemResource.getInputStream().readAllBytes();
        assertEquals(0, dataFromFile.length);
    }

    @Test
    @Order(50)
    void deleteAll() throws IOException {
        Files.delete(
                Paths.get(
                        dirUploadedFiles,
                        accountId.toString(),
                        emptyFileId.toString()
                )
        );
        Files.delete(
                Paths.get(
                        dirUploadedFiles,
                        accountId.toString(),
                        fileId.toString()
                )
        );
        Files.delete(Paths.get(dirUploadedFiles, accountId.toString()));
        Files.delete(Paths.get(dirUploadedFiles));
    }
}
