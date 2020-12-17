package ru.itterminal.botdesk.files.controller;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Random;
import java.util.UUID;

import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
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

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.commons.exception.RestExceptionHandler;
import ru.itterminal.botdesk.files.model.File;
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

    private static final String HOST = "http://localhost";
    private static final String PORT = ":8081";
    private static final String API = "api/v1/file-data";

    private final byte[] fileData = new byte[10];
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
        File file = File.builder()
                .createdAt(System.currentTimeMillis())
                .entityId(ENTITY_ID)
                .fileName(FILE_NAME)
                .size(FILE_SIZE)
                .account(account)
                .build();
        file.setId(FILE_ID);
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

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void putFileData_shouldGetStatusOk_whenPassedValidData() throws Exception {
        when(accountService.findById(any())).thenReturn(account);
        when(fileService.putFileData(account.getId(), FILE_ID, fileData)).thenReturn(true);
        mockMvc.perform(MockMvcRequestBuilders.multipart(HOST + PORT + API)
                                .file(mockMultipartFile)
                                .param("fileId", String.valueOf(FILE_ID)))
                .andDo(print())
                .andExpect(status().isOk());
        verify(accountService, times(1)).findById(any());
        verify(fileService, times(1)).putFileData(account.getId(), FILE_ID, fileData);
    }

    @Test
    @WithAnonymousUser
    void putFileData_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        mockMvc.perform(post(HOST + PORT + API + "?fileId=" + FILE_ID))
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(accountService, times(0)).findById(any());
        verify(fileService, times(0)).putFileData(any(), any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void putFileData_shouldGetStatusBadRequest_whenFileIdIsInvalid() throws Exception {
        mockMvc.perform(MockMvcRequestBuilders.multipart(HOST + PORT + API)
                                .file(mockMultipartFile)
                                .param("fileId", FILE_ID + "abracadabra"))
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.title").value(REQUEST_NOT_READABLE))
                .andExpect(jsonPath("$.detail").value(Matchers.containsString(
                        "Failed to convert value of type 'java.lang.String' to required type 'java.util.UUID'")));
        verify(accountService, times(0)).findById(any());
        verify(fileService, times(0)).putFileData(any(), any(), any());
    }
}