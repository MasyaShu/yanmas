package ru.itterminal.yanmas.files.controller;

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
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.aau.util.ReflectionHelper;
import ru.itterminal.yanmas.commons.exception.RestExceptionHandler;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.files.service.FileServiceImpl;
import ru.itterminal.yanmas.security.config.TestSecurityConfig;
import ru.itterminal.yanmas.security.jwt.JwtUser;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;

import java.util.Random;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {FileControllerV1.class, FilterChainProxy.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class FileControllerV1Test {

    @MockBean
    private FileServiceImpl fileService;

    @MockBean
    JwtUserBuilder jwtUserBuilder;

    @MockBean
    UserServiceImpl userService;

    @MockBean
    SpecificationsFactory specificationsFactory;

    @SuppressWarnings("unused")
    @MockBean
    private ReflectionHelper reflectionHelper;

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

    private final byte[] fileData = new byte[10];
    private static final UUID FILE_ID = UUID.fromString("813b0453-9f71-45e4-b2fa-282e4e6dc90c");
    private static final String REQUEST_NOT_READABLE = "Request not readable";

    MockMultipartFile mockMultipartFile;


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

    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getFileData_shouldGetFileData_whenPassedValidParameters() throws Exception {
        when(jwtUserBuilder.getJwtUser()).thenReturn(new JwtUser());
        when(userService.findById(any())).thenReturn(new User());
        mockMvc.perform(get(HOST + PORT + API + "/" + FILE_ID + "/data"))
                .andDo(print())
                .andExpect(status().isOk())
                .andReturn();
        verify(fileService, times(1)).getFileData(any(), any());
    }

    @Test
    @WithAnonymousUser
    void getFileData_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        mockMvc.perform(get(HOST + PORT + API + "/" + FILE_ID + "/data"))
                .andDo(print())
                .andExpect(status().isUnauthorized());
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
        when(jwtUserBuilder.getJwtUser()).thenReturn(new JwtUser());
        when(userService.findById(any())).thenReturn(new User());
        mockMvc.perform(MockMvcRequestBuilders.multipart(HOST + PORT + API + "/" + FILE_ID + "/data")
                                .file(mockMultipartFile))
                .andDo(print())
                .andExpect(status().isOk());
        verify(fileService, times(1)).putFileData(any(), any(), any());
    }

    @Test
    @WithAnonymousUser
    void putFileData_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        mockMvc.perform(post(HOST + PORT + API + "/" + FILE_ID + "/data"))
                .andDo(print())
                .andExpect(status().isUnauthorized());
        verify(fileService, times(0)).putFileData( any(), any(), any());
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
        verify(fileService, times(0)).putFileData(any(), any(), any());
    }

}
