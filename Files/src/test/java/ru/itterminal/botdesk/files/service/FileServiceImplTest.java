package ru.itterminal.botdesk.files.service;

import static java.lang.String.format;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createExpectedLogicalValidationException;
import static ru.itterminal.botdesk.files.service.FileServiceImpl.FILE_ID;
import static ru.itterminal.botdesk.files.service.FileServiceImpl.FILE_ID_IS_NULL;
import static ru.itterminal.botdesk.files.service.FileServiceImpl.MAX_SIZE;
import static ru.itterminal.botdesk.files.service.FileServiceImpl.SIZE_FILE;

import java.util.Optional;
import java.util.Random;
import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.files.model.File;
import ru.itterminal.botdesk.files.repository.FileRepository;
import ru.itterminal.botdesk.files.service.validator.FileOperationValidator;
import ru.itterminal.botdesk.integration.aws.s3.AwsS3ObjectOperations;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.security.jwt.JwtUserBuilder;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {FileServiceImpl.class})
@TestPropertySource(properties = {"maxSizeOfFile=26214400"})
class FileServiceImplTest {

    public static final String MAX_SIZE_OF_FILE = "26214400";

    @MockBean
    private FileRepository repository;

    @MockBean
    private JwtUserBuilder jwtUserBuilder;

    @MockBean
    private JwtUser jwtUser;

    @SuppressWarnings("unused")
    @MockBean
    private FileOperationValidator validator;

    @SuppressWarnings("unused")
    @MockBean
    private AccountServiceImpl accountService;

    @MockBean
    private AwsS3ObjectOperations awsS3ObjectOperations;

    @Autowired
    private FileServiceImpl service;

    private final byte[] fileData = new byte[10];
    private File file;
    private Account account;

    @BeforeAll
    void setupBeforeAll() {
        new Random().nextBytes(fileData);
        account = Account.builder()
                .id(UUID.randomUUID())
                .build();
        file = File.builder()
                .account(account)
                .authorId(UUID.randomUUID())
                .fileName("file_name")
                .createdAt(1606967833L)
                .isUploaded(true)
                .build();
        file.setId(UUID.randomUUID());
    }

    @Test
    void getFileData_shouldGetBytesOfFile_whenAccordingWithPlannedBehavior() {
        when(jwtUserBuilder.getJwtUser()).thenReturn(jwtUser);
        when(jwtUser.getAccountId()).thenReturn(UUID.randomUUID());
        when(repository.existsById(any())).thenReturn(true);
        when(repository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(file));
        when(awsS3ObjectOperations.getObject(any(), any())).thenReturn(fileData);
        byte[] actualBytesOfFile = service.getFileData(UUID.randomUUID(), UUID.randomUUID());
        assertEquals(fileData, actualBytesOfFile);
        verify(awsS3ObjectOperations, times(1)).getObject(any(), any());
        verify(repository, times(1)).findByIdAndAccountId(any(), any());
        verify(repository, times(1)).existsById(any());
    }

    @Test
    void putFileData_shouldGetStatusTrue_whenAccordingWithPlannedBehavior() {
        when(repository.findByAccountIdAndAuthorIdAndId(any(), any(), any())).thenReturn(Optional.of(file));
        when(repository.update(any())).thenReturn(file);
        when(awsS3ObjectOperations.putObject(any(), any(), any())).thenReturn(true);
        boolean actualStatus = service.putFileData(account.getId(),file.getAuthorId(), file.getId(), fileData);
        assertTrue(actualStatus);
        verify(repository, times(0)).existsById(any());
        verify(repository, times(1)).findByAccountIdAndAuthorIdAndId(any(), any(), any());
        verify(repository, times(1)).update(any());
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
                () -> service.putFileData(accountId, null, null, fileData)
        );
        assertEquals(
                expectedException.getFieldErrors().get(FILE_ID).get(0),
                actualException.getFieldErrors().get(FILE_ID).get(0)
        );
        verify(repository, times(0)).findByAccountIdAndAuthorIdAndId(any(), any(), any());
        verify(repository, times(0)).save(any());
        verify(awsS3ObjectOperations, times(0)).putObject(any(), any(), any());
    }

    @Test
    void putFileData_shouldGetEntityNotExistException_whenCantFindEntityByPassedParameters() {
        when(repository.findByAccountIdAndAuthorIdAndId(any(), any(), any())).thenReturn(Optional.empty());
        UUID fileId = UUID.randomUUID();
        UUID authorId = UUID.randomUUID();
        UUID accountId = UUID.randomUUID();
        assertThrows(
                EntityNotExistException.class,
                () -> service.putFileData(accountId, authorId, fileId, fileData)
        );
        verify(repository, times(1)).findByAccountIdAndAuthorIdAndId(any(), any(), any());
        verify(repository, times(0)).findByIdAndAccountId(any(), any());
        verify(repository, times(0)).save(any());
        verify(awsS3ObjectOperations, times(0)).putObject(any(), any(), any());
    }

    @Test
    void putFileData_shouldGetLogicalValidationException_whenSizeOfFileMoreThan25Mb() {
        var expectedException = createExpectedLogicalValidationException(SIZE_FILE, format(MAX_SIZE, MAX_SIZE_OF_FILE));
        UUID fileId = UUID.randomUUID();
        UUID accountId = UUID.randomUUID();
        UUID authorId = UUID.randomUUID();
        byte[] fileData = new byte[27214400];
        var actualException = assertThrows(
                LogicalValidationException.class,
                () -> service.putFileData(accountId, authorId, fileId, fileData)
        );

        assertEquals(
                expectedException.getFieldErrors().get(SIZE_FILE).get(0),
                actualException.getFieldErrors().get(SIZE_FILE).get(0)
        );
    }
}