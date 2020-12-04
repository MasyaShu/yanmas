package ru.itterminal.botdesk.files.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.Random;
import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.files.model.File;
import ru.itterminal.botdesk.files.repository.FileRepository;
import ru.itterminal.botdesk.files.service.validator.FileOperationValidator;
import ru.itterminal.botdesk.integration.aws.s3.AwsS3ObjectOperations;
import ru.itterminal.botdesk.integration.aws.s3.flow.PutAwsS3ObjectFlow;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {FileServiceImpl.class})
class FileServiceImplTest {

    @MockBean
    private FileRepository repository;

    @MockBean
    private FileOperationValidator validator;

    @SuppressWarnings("unused")
    @MockBean
    private PutAwsS3ObjectFlow.PutAwsS3ObjectGateway putAwsS3ObjectGateway;

    @MockBean
    private AwsS3ObjectOperations awsS3ObjectOperations;

    @Autowired
    private FileServiceImpl service;

    private final byte[] fileData = new byte[10];
    private File file;

    @BeforeAll
    void setupBeforeAll() {
        new Random().nextBytes(fileData);
        Account account = Account.builder().build();
        account.setId(UUID.randomUUID());
        file = File.builder()
                .account(account)
                .fileName("file_name")
                .createdAt(1606967833L)
                .build();
    }

    @Test
    void create_shouldGetUnsupportedOperationException_whenCallMethodWithParameterFile () {
        File file = File.builder().build();
        assertThrows(UnsupportedOperationException.class,
                     ()-> service.create(file));
    }

    @Test
    void update_shouldGetUnsupportedOperationException_whenCallMethodUpdate () {
        File file = File.builder().build();
        assertThrows(UnsupportedOperationException.class,
                     ()-> service.update(file));
    }

    @Test
    void create_shouldGetFile_whenPassedValidData () {
        when(repository.create(any())).thenReturn(file);
        File createdFile = service.create(file, fileData);
        assertEquals(file, createdFile);
    }
}