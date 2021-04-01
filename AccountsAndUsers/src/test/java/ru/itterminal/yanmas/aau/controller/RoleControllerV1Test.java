package ru.itterminal.yanmas.aau.controller;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import ru.itterminal.yanmas.aau.model.Role;
import ru.itterminal.yanmas.aau.service.impl.RoleServiceImpl;
import ru.itterminal.yanmas.commons.exception.RestExceptionHandler;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(RoleControllerV1.class)
@WebMvcTest
class RoleControllerV1Test {

    @MockBean
    private RoleServiceImpl service;

    private static final String HOST = "http://localhost";
    private static final String PORT = ":8081";
    private static final String API = "api/v1/role/";
    private static final String ROLE_NAME_1 = "TestNameOfRole_1";
    private static final String ROLE_NAME_2 = "TestNameOfRole_2";
    private static final String ROLE_UUID_1 = "d45e0a6e-cb5b-11ea-87d0-0242ac138003";
    private static final String ROLE_UUID_2 = "bd61e166-cc30-11ea-87d0-0242ac130003";
    private Role role_1;
    private Role role_2;

    @Autowired
    private RoleControllerV1 controller;

    private MockMvc mockMvc;

    @BeforeAll
    void setUpBeforeAll() {
        mockMvc = MockMvcBuilders.standaloneSetup(controller)
                .setControllerAdvice(new RestExceptionHandler())
                .build();
    }

    @BeforeEach
    void setUpBeforeEach() {
        role_1 = Role.builder().name(ROLE_NAME_1).build();
        role_1.setId(UUID.fromString(ROLE_UUID_1));
        role_2 = Role.builder().name(ROLE_NAME_2).build();
        role_2.setId(UUID.fromString(ROLE_UUID_2));
    }


    @Test
    void getAll_shouldGetTwoRoles_whenDataIsExist() throws Exception {
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
    void getAll_shouldEmptyList_whenDataIsNotExist() throws Exception {
        when(service.findAll()).thenReturn(Collections.emptyList());
        mockMvc.perform(get(HOST + PORT + API))
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").isEmpty());
        verify(service, times(1)).findAll();
    }

}