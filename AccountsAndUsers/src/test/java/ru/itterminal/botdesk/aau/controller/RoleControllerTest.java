package ru.itterminal.botdesk.aau.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_NOT_BE_NULL;

import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;

import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.dto.RoleDto;
import ru.itterminal.botdesk.aau.service.impl.RoleServiceImpl;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(RoleController.class)
@WebMvcTest
class RoleControllerTest {

    @MockBean
    private RoleServiceImpl service;

    private static String HOST = "http://localhost";
    private static String PORT = ":8081";
    private static String API = "/v1/role/";
    private static String ROLE_NAME_1 = "TestNameOfRole_1";
    private static String ROLE_NAME_2 = "TestNameOfRole_2";
    private static String ROLE_UUID_1 = "d45e0a6e-cb5b-11ea-87d0-0242ac138003";
    private static String ROLE_UUID_2 = "bd61e166-cc30-11ea-87d0-0242ac130003";
    private ObjectMapper objectMapper = new ObjectMapper();
    private RoleDto roleDto;
    private Role role;

    @Autowired
    private RoleController controller;

    private MockMvc mockMvc;

    @BeforeAll
    void setUpBeforeAll() {
        MockitoAnnotations.initMocks(this);
        mockMvc = MockMvcBuilders.standaloneSetup(controller)
                .setControllerAdvice(WebTestConfig.controllerAdvice())
                .build();
    }

    @BeforeEach
    void setUpBeforeEach() {
        roleDto = new RoleDto().builder().name(ROLE_NAME_1).build();
        roleDto.setDeleted(false);
        role = new Role().builder().name(ROLE_NAME_1).build();
        role.setId(UUID.fromString(ROLE_UUID_1));
    }

    @Test
    public void create_shouldCreate_whenValidDataPassed() throws Exception {
        when(service.create(any())).thenReturn(role);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(roleDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.id").value(ROLE_UUID_1))
                .andExpect(jsonPath("$.name").value(ROLE_NAME_1));
    }

    @Test
    public void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        roleDto.setName(null);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(roleDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errors.name[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists());
        verify(service, times(0)).create(any());
    }

}