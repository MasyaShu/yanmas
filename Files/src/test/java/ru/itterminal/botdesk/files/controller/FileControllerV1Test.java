package ru.itterminal.botdesk.files.controller;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.List;
import java.util.Random;
import java.util.UUID;

import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.test.context.support.WithAnonymousUser;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers;
import org.springframework.security.web.FilterChainProxy;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.commons.exception.RestExceptionHandler;
import ru.itterminal.botdesk.files.model.File;
import ru.itterminal.botdesk.files.model.FileDto;
import ru.itterminal.botdesk.files.service.FileServiceImpl;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {FileControllerV1.class, FilterChainProxy.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles("Test")
class FileControllerV1Test {

    @MockBean
    private FileServiceImpl fileService;

    @MockBean
    private AccountServiceImpl accountService;

    @Autowired
    private FileControllerV1 controller;

    @Autowired
    FilterChainProxy springSecurityFilterChain;

    @Autowired
    UserDetailsService userDetailsService;

    private MockMvc mockMvc;

    private final ObjectMapper objectMapper = new ObjectMapper();
    private final ModelMapper modelMapper = new ModelMapper();
    private static final String HOST = "http://localhost";
    private static final String PORT = ":8081";
    private static final String API = "api/v1/file";

    private final byte[] fileData = new byte[10];
    private File file;
    private FileDto fileDto;
    private Account account;
    private static final UUID ENTITY_ID = UUID.fromString("3d4ff7a6-9c73-4d69-bdfd-5b0951823460");
    private static final UUID ACCOUNT_ID = UUID.fromString("caef5dd3-a488-4c68-92cb-ad8f66976027");
    private static final UUID FILE_ID = UUID.fromString("813b0453-9f71-45e4-b2fa-282e4e6dc90c");
    private static final String FILE_NAME = "File name.txt";
    private static final String ACCOUNT_NAME = "Account name";
    private static final int FILE_SIZE = 10;
    MockMultipartFile mockMultipartFile;
    private static final String REQUEST_NOT_READABLE = "Request not readable";

    @BeforeAll
    void setUpBeforeAll() {
        mockMvc = MockMvcBuilders.standaloneSetup(controller)
                .setControllerAdvice(new RestExceptionHandler())
                .apply(SecurityMockMvcConfigurers.springSecurity(springSecurityFilterChain))
                .build();
        new Random().nextBytes(fileData);
        mockMultipartFile = new MockMultipartFile("file", fileData);
        account = Account.builder()
                .name(ACCOUNT_NAME)
                .build();
        account.setId(ACCOUNT_ID);
        file = File.builder()
                .createdAt(System.currentTimeMillis())
                .entityId(ENTITY_ID)
                .fileName(FILE_NAME)
                .size(FILE_SIZE)
                .account(account)
                .build();
        file.setId(FILE_ID);
        fileDto = modelMapper.map(file, FileDto.class);
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldCreateFile_whenPassedValidParameters() throws Exception {
        when(fileService.create(any(), any())).thenReturn(file);
        when(accountService.findById(any())).thenReturn(account);
        mockMvc.perform(MockMvcRequestBuilders.multipart(HOST + PORT + API)
                                .file(mockMultipartFile)
                                .param("entityId", file.getEntityId().toString())
                                .param("fileName", file.getFileName()))
                .andDo(print())
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.id").value(file.getId().toString()))
                .andExpect(jsonPath("$.entityId").value(file.getEntityId().toString()))
                .andExpect(jsonPath("$.fileName").value(file.getFileName()))
                .andExpect(jsonPath("$.size").value(file.getSize()))
                .andExpect(jsonPath("$.createdAt").value(file.getCreatedAt()));
        verify(fileService, times(1)).create(any(), any());
        verify(accountService, times(1)).findById(any());
    }

    @Test
    @WithAnonymousUser
    void create_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        mockMvc.perform(MockMvcRequestBuilders.multipart(HOST + PORT + API)
                                .file(mockMultipartFile)
                                .param("entityId", file.getEntityId().toString())
                                .param("fileName", file.getFileName()))
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(fileService, times(0)).create(any(), any());
        verify(accountService, times(0)).findById(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequest_whenFilenameIsEmpty() throws Exception {
        mockMvc.perform(MockMvcRequestBuilders.multipart(HOST + PORT + API)
                                .file(mockMultipartFile)
                                .param("entityId", file.getEntityId().toString())
                                .param("fileName", ""))
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.title").value(REQUEST_NOT_READABLE))
                .andExpect(jsonPath("$.detail").value("create.fileName: must not be empty"));
        verify(fileService, times(0)).create(any(), any());
        verify(accountService, times(0)).findById(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequest_whenEntityIdIsInvalid() throws Exception {
        mockMvc.perform(MockMvcRequestBuilders.multipart(HOST + PORT + API)
                                .file(mockMultipartFile)
                                .param("entityId", file.getEntityId().toString() + "abracadabra")
                                .param("fileName", file.getFileName()))
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.title").value(REQUEST_NOT_READABLE))
                .andExpect(jsonPath("$.detail").value(Matchers.containsString(
                        "Failed to convert value of type 'java.lang.String' to required type 'java.util.UUID'")));
        verify(fileService, times(0)).create(any(), any());
        verify(accountService, times(0)).findById(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getAllByEntityIdAndAccountId_shouldGetListOfFileDto_whenPassedValidParameters() throws Exception {
        when(accountService.findById(any())).thenReturn(account);
        when(fileService.findAllByEntityIdAndAccountId(account.getId(), ENTITY_ID)).thenReturn(List.of(file));
        mockMvc.perform(get(HOST + PORT + API + "/list?entityId=" + ENTITY_ID.toString()))
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.[0].id").value(fileDto.getId().toString()))
                .andExpect(jsonPath("$.[0].size").value(fileDto.getSize()))
                .andExpect(jsonPath("$.[0].createdAt").value(fileDto.getCreatedAt()))
                .andExpect(jsonPath("$.[0].fileName").value(fileDto.getFileName()));
        verify(accountService, times(1)).findById(any());
        verify(fileService, times(1)).findAllByEntityIdAndAccountId(any(), any());
    }

    @Test
    @WithAnonymousUser
    void getAllByEntityIdAndAccountId_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        mockMvc.perform(get(HOST + PORT + API + "/list?entityId=" + ENTITY_ID.toString()))
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(accountService, times(0)).findById(any());
        verify(fileService, times(0)).findAllByEntityIdAndAccountId(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getAllByEntityIdAndAccountId_shouldGetStatusBadRequest_whenEntityIdIsInvalid() throws Exception {
        mockMvc.perform(get(HOST + PORT + API + "/list?entityId=" + ENTITY_ID.toString() + "abracadabra"))
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.title").value(REQUEST_NOT_READABLE))
                .andExpect(jsonPath("$.detail").value(Matchers.containsString(
                        "Failed to convert value of type 'java.lang.String' to required type 'java.util.UUID'")));
        verify(accountService, times(0)).findById(any());
        verify(fileService, times(0)).findAllByEntityIdAndAccountId(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getFileData_shouldGetFileData_whenPassedValidParameters() throws Exception {
        when(accountService.findById(any())).thenReturn(account);
        when(fileService.getFileData(account.getId(), FILE_ID)).thenReturn(fileData);
        var result = mockMvc.perform(get(HOST + PORT + API + "?fileId=" + FILE_ID))
                .andDo(print())
                .andExpect(status().isOk())
                .andReturn();
        byte[] actualFileData = result.getResponse().getContentAsByteArray();
        assertArrayEquals(fileData, actualFileData);
        verify(accountService, times(1)).findById(any());
        verify(fileService, times(1)).getFileData(account.getId(), FILE_ID);
    }

    @Test
    @WithAnonymousUser
    void getFileData_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        mockMvc.perform(get(HOST + PORT + API + "?fileId=" + FILE_ID))
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(accountService, times(0)).findById(any());
        verify(fileService, times(0)).getFileData(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getFileData_shouldGetStatusBadRequest_whenFileIdIsInvalid() throws Exception {
        mockMvc.perform(get(HOST + PORT + API + "?fileId=" + FILE_ID + "abracadabra"))
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.title").value(REQUEST_NOT_READABLE))
                .andExpect(jsonPath("$.detail").value(Matchers.containsString(
                        "Failed to convert value of type 'java.lang.String' to required type 'java.util.UUID'")));
        verify(accountService, times(0)).findById(any());
        verify(fileService, times(0)).getFileData(any(), any());
    }

}