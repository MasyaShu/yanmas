package ru.itterminal.botdesk.files.controller;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

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
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.test.context.support.WithAnonymousUser;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers;
import org.springframework.security.web.FilterChainProxy;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.exception.RestExceptionHandler;
import ru.itterminal.botdesk.commons.util.CommonConstants;
import ru.itterminal.botdesk.files.model.File;
import ru.itterminal.botdesk.files.model.dto.FileDto;
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

    @Autowired
    private FileControllerV1 controller;

    @Autowired
    FilterChainProxy springSecurityFilterChain;

    @Autowired
    UserDetailsService userDetailsService;

    private MockMvc mockMvc;

    private static final String HOST = "http://localhost";
    private static final String PORT = ":8081";
    private static final String API = "api/v1/file";
    private final ModelMapper mapper = new ModelMapper();
    private final ObjectMapper objectMapper = new ObjectMapper();

    private final byte[] fileData = new byte[10];
    private static final UUID ENTITY_ID = UUID.fromString("3d4ff7a6-9c73-4d69-bdfd-5b0951823460");
    private static final UUID ACCOUNT_ID = UUID.fromString("caef5dd3-a488-4c68-92cb-ad8f66976027");
    private static final UUID FILE_ID = UUID.fromString("813b0453-9f71-45e4-b2fa-282e4e6dc90c");
    private static final String FILE_NAME = "File name.txt";
    private static final String ACCOUNT_NAME = "Account name";
    private static final int FILE_SIZE = 10;
    private static final String REQUEST_NOT_READABLE = "Request not readable";

    MockMultipartFile mockMultipartFile;

    private File file;
    private FileDto fileDto;

    @BeforeAll
    void setUpBeforeAll() {
        mockMvc = MockMvcBuilders.standaloneSetup(controller)
                .setControllerAdvice(new RestExceptionHandler())
                .apply(SecurityMockMvcConfigurers.springSecurity(springSecurityFilterChain))
                .addFilter((request, response, chain) -> {
                    response.setCharacterEncoding("UTF-8"); // this is crucial
                    chain.doFilter(request, response);
                }, "/*")
                .build();
        new Random().nextBytes(fileData);
        mockMultipartFile = new MockMultipartFile("file", fileData);
        Account account = Account.builder()
                .id(ACCOUNT_ID)
                .name(ACCOUNT_NAME)
                .build();
        file = File.builder()
                .id(FILE_ID)
                .createdAt(System.currentTimeMillis())
                .entityId(ENTITY_ID)
                .fileName(FILE_NAME)
                .size(FILE_SIZE)
                .account(account)
                .build();
        fileDto = FileDto.builder()
                .fileName(FILE_NAME)
                .size(FILE_SIZE)
                .build();
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldCreate_whenValidDataPassed() throws Exception {
        fileDto.setSize(null);
        when(fileService.create(any())).thenReturn(file);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(fileDto));
        var requestResult = mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isCreated())
                .andReturn()
                .getResponse()
                .getContentAsString();
        var actualTicketDtoResponse = objectMapper.readValue(requestResult, FileDto.class);
        var expectedTicketDtoResponse = mapper.map(file, FileDto.class);
        assertEquals(expectedTicketDtoResponse, actualTicketDtoResponse);
        verify(fileService, times(1)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        fileDto.setFileName(null);
        fileDto.setSize(FILE_SIZE);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(fileDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.fileName[?(@.message == '%s')]",
                                           CommonConstants.MUST_NOT_BE_NULL
                                   ).exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.size[?(@.message == '%s')]",
                                           CommonConstants.MUST_BE_NULL
                                   ).exists());
        verify(fileService, times(0)).create(any());

    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldCreate_whenValidDataPassed() throws Exception {
        fileDto.setId(UUID.randomUUID());
        fileDto.setVersion(0);
        fileDto.setDeleted(false);
        fileDto.setFileName(null);
        fileDto.setSize(null);
        when(fileService.update(any())).thenReturn(file);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(fileDto));
        var requestResult = mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andReturn()
                .getResponse()
                .getContentAsString();
        var actualTicketDtoResponse = objectMapper.readValue(requestResult, FileDto.class);
        var expectedTicketDtoResponse = mapper.map(file, FileDto.class);
        assertEquals(expectedTicketDtoResponse, actualTicketDtoResponse);
        verify(fileService, times(1)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        fileDto.setId(UUID.randomUUID());
        fileDto.setVersion(0);
        fileDto.setDeleted(false);
        fileDto.setFileName(FILE_NAME);
        fileDto.setSize(FILE_SIZE);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(fileDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.fileName[?(@.message == '%s')]",
                                           CommonConstants.MUST_BE_NULL
                                   ).exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.size[?(@.message == '%s')]",
                                           CommonConstants.MUST_BE_NULL
                                   ).exists());
        verify(fileService, times(0)).update(any());

    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getFileData_shouldGetFileData_whenPassedValidParameters() throws Exception {
        when(fileService.getFileData(any(), any())).thenReturn(fileData);
        var result = mockMvc.perform(get(HOST + PORT + API + "/" + FILE_ID + "/data"))
                .andDo(print())
                .andExpect(status().isOk())
                .andReturn();
        byte[] actualFileData = result.getResponse().getContentAsByteArray();
        assertArrayEquals(fileData, actualFileData);
        verify(fileService, times(1)).getFileData(any(), any());
    }

    @Test
    @WithAnonymousUser
    void getFileData_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        mockMvc.perform(get(HOST + PORT + API + "/" + FILE_ID + "/data"))
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(fileService, times(0)).getFileData(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getFileData_shouldGetStatusBadRequest_whenFileIdIsInvalid() throws Exception {
        mockMvc.perform(get(HOST + PORT + API + "/" + FILE_ID + "abracadabra" + "/data"))
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.title").value(REQUEST_NOT_READABLE))
                .andExpect(jsonPath("$.detail").value(Matchers.containsString(
                        "Failed to convert value of type 'java.lang.String' to required type 'java.util.UUID'")));
        verify(fileService, times(0)).getFileData(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void putFileData_shouldGetStatusOk_whenPassedValidData() throws Exception {
        when(fileService.putFileData(any(), any(), any(), any())).thenReturn(true);
        mockMvc.perform(MockMvcRequestBuilders.multipart(HOST + PORT + API + "/" + FILE_ID + "/data")
                                .file(mockMultipartFile))
                .andDo(print())
                .andExpect(status().isOk());
        verify(fileService, times(1)).putFileData(any(), any(), any(), any());
    }

    @Test
    @WithAnonymousUser
    void putFileData_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        mockMvc.perform(post(HOST + PORT + API + "/" + FILE_ID + "/data"))
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(fileService, times(0)).putFileData(any(), any(), any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void putFileData_shouldGetStatusBadRequest_whenFileIdIsInvalid() throws Exception {
        mockMvc.perform(MockMvcRequestBuilders.multipart(HOST + PORT + API + "/" + FILE_ID + "abracadabra" + "/data")
                                .file(mockMultipartFile))
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.title").value(REQUEST_NOT_READABLE))
                .andExpect(jsonPath("$.detail").value(Matchers.containsString(
                        "Failed to convert value of type 'java.lang.String' to required type 'java.util.UUID'")));
        verify(fileService, times(0)).putFileData(any(), any(), any(), any());
    }

}