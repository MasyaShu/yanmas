package ru.itterminal.botdesk.aau.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_BE_GREATER_THAN_OR_EQUAL_TO_0;
import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY;
import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_NOT_BE_NULL;

import java.util.Collections;
import java.util.List;
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
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;

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
    private Role role_1;
    private Role role_2;

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
        role_1 = new Role().builder().name(ROLE_NAME_1).build();
        role_1.setId(UUID.fromString(ROLE_UUID_1));
        role_2 = new Role().builder().name(ROLE_NAME_2).build();
        role_2.setId(UUID.fromString(ROLE_UUID_2));
    }

    @Test
    public void create_shouldCreate_whenValidDataPassed() throws Exception {
        when(service.create(any())).thenReturn(role_1);
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

    @Test
    public void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenAllPassedDataIsNull() throws Exception {
        roleDto.setName(null);
        roleDto.setDeleted(null);
        roleDto.setId(null);
        roleDto.setVersion(null);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(roleDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errors.name[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.deleted[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists());
        verify(service, times(0)).create(any());
    }

    @Test
    public void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenIdAndVersionNotNull() throws Exception {
        roleDto.setId(UUID.randomUUID());
        roleDto.setVersion(15);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(roleDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errors.id[?(@.message == '%s')]", MUST_BE_NULL_FOR_THE_NEW_ENTITY).exists())
                .andExpect(
                        jsonPath("$.errors.version[?(@.message == '%s')]", MUST_BE_NULL_FOR_THE_NEW_ENTITY).exists());
        verify(service, times(0)).create(any());
    }

    @Test
    public void update_shouldCreate_whenValidDataPassed() throws Exception {
        roleDto.setId(UUID.fromString(ROLE_UUID_1));
        roleDto.setVersion(0);
        role_1.setVersion(0);
        when(service.update(any())).thenReturn(role_1);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(roleDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.id").value(ROLE_UUID_1))
                .andExpect(jsonPath("$.name").value(ROLE_NAME_1));
    }

    @Test
    public void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        roleDto.setName(null);
        roleDto.setId(UUID.fromString(ROLE_UUID_1));
        roleDto.setVersion(0);
        role_1.setVersion(0);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(roleDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errors.name[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists());
        verify(service, times(0)).create(any());
    }

    @Test
    public void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenAllPassedDataIsNull() throws Exception {
        roleDto.setName(null);
        roleDto.setDeleted(null);
        roleDto.setId(null);
        roleDto.setVersion(null);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(roleDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errors.name[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.id[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.version[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.deleted[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists());
        verify(service, times(0)).create(any());
    }

    @Test
    public void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenVersionIsNegative() throws Exception {
        roleDto.setId(UUID.fromString(ROLE_UUID_1));
        roleDto.setVersion(-1);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(roleDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errors.version[?(@.message == '%s')]", MUST_BE_GREATER_THAN_OR_EQUAL_TO_0)
                        .exists());
        verify(service, times(0)).create(any());
    }

    @Test
    public void getById_shouldFindOneRole_whenRoleExistInDatabaseByPassedId() throws Exception {
        when(service.findById(UUID.fromString(ROLE_UUID_1))).thenReturn(role_1);
        mockMvc.perform(get(HOST + PORT + API + ROLE_UUID_1))
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.name").value(ROLE_NAME_1))
                .andExpect(jsonPath("$.id").value(ROLE_UUID_1));
        verify(service, times(1)).findById(any());
    }

    @Test
    void getById_shouldRespondNotFound_whenPassedIdNotExist() throws Exception {
        when(service.findById(UUID.fromString(ROLE_UUID_1))).thenThrow(EntityNotExistException.class);
        mockMvc.perform(get(HOST + PORT + API + ROLE_UUID_1))
                .andDo(print())
                .andExpect(status().isNotFound());
        verify(service, times(1)).findById(any());
    }

    @Test
    public void getById_shouldGetStatusBadRequest_whenUiidIsInvalid() throws Exception {
        mockMvc.perform(get(HOST + PORT + API + ROLE_UUID_1 + "Abracadabra"))
                .andDo(print())
                .andExpect(status().isBadRequest());
        verify(service, times(0)).findById(any());
    }

    @Test
    public void getAll_shouldGetTwoRoles_whenDataIsExist() throws Exception {
        when(service.findAll()).thenReturn(List.of(role_1, role_2));
        mockMvc.perform(get(HOST + PORT + API))
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$[?(@.id == '%s')]", ROLE_UUID_1).exists())
                .andExpect(jsonPath("$[?(@.id == '%s')]", ROLE_UUID_2).exists())
                .andExpect(jsonPath("$[?(@.name == '%s')]", ROLE_NAME_1).exists())
                .andExpect(jsonPath("$[?(@.name == '%s')]", ROLE_NAME_2).exists());
        verify(service, times(1)).findAll();
    }

    @Test
    public void getAll_shouldEmptyList_whenDataIsNotExist() throws Exception {
        when(service.findAll()).thenReturn(Collections.emptyList());
        mockMvc.perform(get(HOST + PORT + API))
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").isEmpty());
        verify(service, times(1)).findAll();
    }

    @Test
    void physicalDelete_shouldThrowUnsupportedOperationException_whenValidDataPassed() throws Exception {
        roleDto.setId(UUID.fromString(ROLE_UUID_1));
        roleDto.setVersion(0);
        MockHttpServletRequestBuilder request = delete(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(roleDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isMethodNotAllowed());
    }
}