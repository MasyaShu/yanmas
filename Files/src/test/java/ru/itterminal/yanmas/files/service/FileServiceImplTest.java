package ru.itterminal.yanmas.files.service;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.List;
import java.util.Optional;
import java.util.Random;
import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.yanmas.aau.model.Account;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.aau.util.ReflectionHelper;
import ru.itterminal.yanmas.commons.exception.EntityNotExistException;
import ru.itterminal.yanmas.files.model.File;
import ru.itterminal.yanmas.files.repository.FileRepository;
import ru.itterminal.yanmas.files.repository.FileSystemRepository;
import ru.itterminal.yanmas.security.jwt.JwtUser;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {FileServiceImpl.class})
class FileServiceImplTest {

    @MockBean
    private FileRepository repository;

    @SuppressWarnings("unused")
    @MockBean
    private ReflectionHelper reflectionHelper;

    @MockBean
    private JwtUserBuilder jwtUserBuilder;

    @MockBean
    private JwtUser jwtUser;

    @MockBean
    private FileSystemRepository fileSystemRepository;

    @SuppressWarnings("unused")
    @MockBean
    private List<EntityValidator<File>> validators;

    @Autowired
    private FileServiceImpl service;

    private final byte[] fileData = new byte[10];
    private File file;
    private User currentUser;

    @BeforeAll
    void setupBeforeAll() {
        new Random().nextBytes(fileData);
        var account = Account.builder()
                .id(UUID.randomUUID())
                .build();
        file = File.builder()
                .account(account)
                .authorId(UUID.randomUUID())
                .fileName("file_name")
                .createdAt(1606967833L)
                .isUploaded(false)
                .build();
        file.setId(UUID.randomUUID());
        currentUser = User.builder()
                .account(account)
                .id(UUID.randomUUID())
                .build();
    }

    @Test
    void getFileData_shouldGetBytesOfFile_whenAccordingWithPlannedBehavior() {
        when(jwtUserBuilder.getJwtUser()).thenReturn(jwtUser);
        when(jwtUser.getAccountId()).thenReturn(UUID.randomUUID());
        when(repository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(file));
        service.getFileData(currentUser, UUID.randomUUID());
        verify(fileSystemRepository, times(1)).findInFileSystem(any());
        verify(repository, times(1)).findByIdAndAccountId(any(), any());
    }

    @Test
    void putFileData_shouldGetStatusTrue_whenAccordingWithPlannedBehavior() throws IOException {
        when(repository.findByAccountIdAndAuthorIdAndId(any(), any(), any())).thenReturn(Optional.of(file));
        when(repository.update(any())).thenReturn(file);
        service.putFileData(currentUser, file.getId(), fileData);
        verify(repository, times(0)).existsById(any());
        verify(repository, times(1)).findByAccountIdAndAuthorIdAndId(any(), any(), any());
        verify(repository, times(1)).update(any());
        verify(fileSystemRepository, times(1)).save(any(), any());
    }

    @Test
    void putFileData_shouldGetEntityNotExistException_whenCantFindEntityByPassedParameters() throws IOException {
        when(repository.findByAccountIdAndAuthorIdAndId(any(), any(), any())).thenReturn(Optional.empty());
        UUID fileId = UUID.randomUUID();
        assertThrows(
                EntityNotExistException.class,
                () -> service.putFileData(currentUser, fileId, fileData)
        );
        verify(repository, times(1)).findByAccountIdAndAuthorIdAndId(any(), any(), any());
        verify(repository, times(0)).findByIdAndAccountId(any(), any());
        verify(repository, times(0)).save(any());
        verify(fileSystemRepository, times(0)).save(any(), any());
    }
}
