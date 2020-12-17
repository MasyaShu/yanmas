package ru.itterminal.botdesk.files.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createExpectedLogicalValidationException;

import java.util.Optional;
import java.util.Random;
import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
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

    @SuppressWarnings("unused")
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
    private Account account;
    private static final String BYTES_OF_FILE = "Bytes of file";
    private static final String BYTES_OF_FILE_IS_NULL = "Bytes of file is null";
    private static final String ACCOUNT_ID = "Account id";
    private static final String ACCOUNT_ID_IS_NULL = "Account id is null";
    private static final String FILE_ID = "File id";
    private static final String FILE_ID_IS_NULL = "File id is null";

    @BeforeAll
    void setupBeforeAll() {
        new Random().nextBytes(fileData);
        account = Account.builder().build();
        account.setId(UUID.randomUUID());
        file = File.builder()
                .account(account)
                .fileName("file_name")
                .createdAt(1606967833L)
                .isUploaded(false)
                .build();
        file.setId(UUID.randomUUID());
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
    void getFileData_shouldGetBytesOfFile_whenAccordingWithPlannedBehavior() {
        when(awsS3ObjectOperations.getObject(any(), any())).thenReturn(fileData);
        byte[] actualBytesOfFile = service.getFileData(UUID.randomUUID(), UUID.randomUUID());
        assertEquals(fileData, actualBytesOfFile);
        verify(awsS3ObjectOperations, times(1)).getObject(any(), any());
    }

    @Test
    void getFileData_shouldGetLogicalValidationException_whenAccountIdIsNull() {
        LogicalValidationException expectedException = createExpectedLogicalValidationException(
                ACCOUNT_ID,
                ACCOUNT_ID_IS_NULL
        );
        UUID fileId = UUID.randomUUID();
        LogicalValidationException actualException = assertThrows(
                LogicalValidationException.class,
                () -> service.getFileData(null, fileId)
        );
        assertEquals(
                expectedException.getFieldErrors().get(ACCOUNT_ID).get(0),
                actualException.getFieldErrors().get(ACCOUNT_ID).get(0)
        );
        verify(awsS3ObjectOperations, times(0)).getObject(any(), any());
    }

    @Test
    void getFileData_shouldGetLogicalValidationException_whenFileIdIsNull() {
        LogicalValidationException expectedException = createExpectedLogicalValidationException(
                FILE_ID,
                FILE_ID_IS_NULL
        );
        UUID accountId = UUID.randomUUID();
        LogicalValidationException actualException = assertThrows(
                LogicalValidationException.class,
                () -> service.getFileData(accountId, null)
        );
        assertEquals(
                expectedException.getFieldErrors().get(FILE_ID).get(0),
                actualException.getFieldErrors().get(FILE_ID).get(0)
        );
        verify(awsS3ObjectOperations, times(0)).getObject(any(), any());
    }

    @Test
    void putFileData_shouldGetStatusTrue_whenAccordingWithPlannedBehavior() {
        when(repository.findByAccountIdAndId(any(), any())).thenReturn(Optional.of(file));
        when(repository.save(any())).thenReturn(file);
        when(awsS3ObjectOperations.putObject(any(), any(), any())).thenReturn(true);
        boolean actualStatus = service.putFileData(account.getId(), file.getId(), fileData);
        assertTrue(actualStatus);
        verify(repository, times(1)).findByAccountIdAndId(any(), any());
        verify(repository, times(1)).save(any());
        verify(awsS3ObjectOperations, times(1)).putObject(any(), any(), any());
    }

    @Test
    void putFileData_shouldGetLogicalValidationException_whenFileIdIsNull() {
        LogicalValidationException expectedException = createExpectedLogicalValidationException(
                FILE_ID,
                FILE_ID_IS_NULL
        );
        UUID accountId = UUID.randomUUID();
        LogicalValidationException actualException = assertThrows(
                LogicalValidationException.class,
                () -> service.putFileData(accountId, null, fileData)
        );
        assertEquals(
                expectedException.getFieldErrors().get(FILE_ID).get(0),
                actualException.getFieldErrors().get(FILE_ID).get(0)
        );
        verify(repository, times(0)).findByAccountIdAndId(any(), any());
        verify(repository, times(0)).save(any());
        verify(awsS3ObjectOperations, times(0)).putObject(any(), any(), any());
    }

    @Test
    void putFileData_shouldGetLogicalValidationException_whenAccountIdIsNull() {
        LogicalValidationException expectedException = createExpectedLogicalValidationException(
                ACCOUNT_ID,
                ACCOUNT_ID_IS_NULL
        );
        UUID fileId = UUID.randomUUID();
        LogicalValidationException actualException = assertThrows(
                LogicalValidationException.class,
                () -> service.putFileData(null, fileId, fileData)
        );
        assertEquals(
                expectedException.getFieldErrors().get(ACCOUNT_ID).get(0),
                actualException.getFieldErrors().get(ACCOUNT_ID).get(0)
        );
        verify(repository, times(0)).findByAccountIdAndId(any(), any());
        verify(repository, times(0)).save(any());
        verify(awsS3ObjectOperations, times(0)).putObject(any(), any(), any());
    }

    @Test
    void putFileData_shouldGetLogicalValidationException_whenFileDataIsNull() {
        LogicalValidationException expectedException = createExpectedLogicalValidationException(
                BYTES_OF_FILE,
                BYTES_OF_FILE_IS_NULL
        );
        UUID fileId = UUID.randomUUID();
        UUID accountId = UUID.randomUUID();
        LogicalValidationException actualException = assertThrows(
                LogicalValidationException.class,
                () -> service.putFileData(accountId, fileId, null)
        );
        assertEquals(
                expectedException.getFieldErrors().get(BYTES_OF_FILE).get(0),
                actualException.getFieldErrors().get(BYTES_OF_FILE).get(0)
        );
        verify(repository, times(0)).findByAccountIdAndId(any(), any());
        verify(repository, times(0)).save(any());
        verify(awsS3ObjectOperations, times(0)).putObject(any(), any(), any());
    }

    @Test
    void putFileData_shouldGetEntityNotExistException_whenCantFindEntityByPassedParameters() {
        when(repository.findByAccountIdAndId(any(), any())).thenReturn(Optional.empty());
        UUID fileId = UUID.randomUUID();
        UUID accountId = UUID.randomUUID();
        assertThrows(EntityNotExistException.class,
                     ()-> service.putFileData(accountId, fileId, fileData));
        verify(repository, times(1)).findByAccountIdAndId(any(), any());
        verify(repository, times(0)).save(any());
        verify(awsS3ObjectOperations, times(0)).putObject(any(), any(), any());
    }
}