package ru.itterminal.botdesk.files.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createExpectedLogicalValidationException;

import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.NullEntityException;
import ru.itterminal.botdesk.files.model.File;
import ru.itterminal.botdesk.files.repository.FileRepository;
import ru.itterminal.botdesk.files.service.validator.FileOperationValidator;
import ru.itterminal.botdesk.integration.aws.s3.AwsS3ObjectOperations;
import ru.itterminal.botdesk.integration.aws.s3.flow.PutAwsS3ObjectFlow;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {FileServiceImpl.class, FileOperationValidator.class})
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
    private static final String BYTES_OF_FILE = "Bytes of file";
    private static final String BYTES_OF_FILE_IS_NULL = "Bytes of file is null";


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
    void create_shouldGetUnsupportedOperationException_whenCallMethodWithParameterFile() {
        File file = File.builder().build();
        assertThrows(
                UnsupportedOperationException.class,
                () -> service.create(file)
        );
    }

    @Test
    void update_shouldGetUnsupportedOperationException_whenCallMethodUpdate() {
        File file = File.builder().build();
        assertThrows(
                UnsupportedOperationException.class,
                () -> service.update(file)
        );
    }

    @Test
    void create_shouldGetFileEntity_whenPassedValidData() {
        when(repository.create(any())).thenReturn(file);
        File createdFile = service.create(file, fileData);
        assertEquals(file, createdFile);
        verify(validator, times(1)).beforeCreate(file);
        verify(validator, times(1)).checkUniqueness(any());
        verify(repository, times(1)).create(any());
        verify(putAwsS3ObjectGateway, times(1)).process(any());
    }

    @Test
    void create_shouldGetNullEntityException_whenFileIsNull() {
        when(validator.beforeCreate(any())).thenThrow(NullEntityException.class);
        assertThrows(NullEntityException.class, () -> service.create(null, fileData));
        verify(validator, times(1)).beforeCreate(any());
        verify(validator, times(0)).checkUniqueness(any());
        verify(repository, times(0)).create(any());
        verify(putAwsS3ObjectGateway, times(0)).process(any());
    }

    @Test
    void create_shouldGetFileEntityWithoutSaveBytesOfFileInAwsS3_whenFileBytesIsEmpty() {
        byte[] emptyFileData = new byte[0];
        when(validator.beforeCreate(any())).thenReturn(true);
        when(repository.create(any())).thenReturn(file);
        File createdFile = service.create(file, emptyFileData);
        assertEquals(file, createdFile);
        verify(validator, times(1)).beforeCreate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(repository, times(1)).create(any());
        verify(putAwsS3ObjectGateway, times(0)).process(any());
    }

    @Test
    void create_shouldGetFileEntityWithoutSaveBytesOfFileInAwsS3_whenFileBytesIsNull() {
        when(validator.beforeCreate(any())).thenReturn(true);
        when(repository.create(any())).thenReturn(file);
        LogicalValidationException expectedException = createExpectedLogicalValidationException(BYTES_OF_FILE, BYTES_OF_FILE_IS_NULL);
        LogicalValidationException actualException = assertThrows(
                LogicalValidationException.class,
                () -> service.create(file, null)
        );
        assertEquals(
                expectedException.getFieldErrors().get(BYTES_OF_FILE).get(0),
                actualException.getFieldErrors().get(BYTES_OF_FILE).get(0)
        );
        verify(validator, times(0)).beforeCreate(any());
        verify(validator, times(0)).checkUniqueness(any());
        verify(repository, times(0)).create(any());
        verify(putAwsS3ObjectGateway, times(0)).process(any());
    }

    @Test
    void findAllByEntityIdAndAccountId_shouldGetEmptyList_whenAccordingWithPlannedBehavior () {
        when(repository.findAllByEntityIdAndAccount_Id(any(), any())).thenReturn(Collections.emptyList());
        List<File> foundedFiles = service.findAllByEntityIdAndAccountId(UUID.randomUUID(), UUID.randomUUID());
        assertTrue(foundedFiles.isEmpty());
        verify(repository, times(1)).findAllByEntityIdAndAccount_Id(any(), any());
    }

    @Test
    void getFileData_shouldGetBytesOfFile_whenAccordingWithPlannedBehavior () {
        when(awsS3ObjectOperations.getObject(any(), any())).thenReturn(fileData);
        byte[] actualBytesOfFile = service.getFileData(UUID.randomUUID().toString(), UUID.randomUUID().toString());
        assertEquals(fileData, actualBytesOfFile);
        verify(awsS3ObjectOperations, times(1)).getObject(any(), any());
    }
}