package ru.itterminal.botdesk.aau.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.test.context.support.WithAnonymousUser;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers;
import org.springframework.security.web.FilterChainProxy;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.dto.AccountCreateDto;
import ru.itterminal.botdesk.aau.model.dto.AccountDto;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.util.AAUConstants;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.exception.RestExceptionHandler;
import ru.itterminal.botdesk.commons.util.CommonConstants;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;

import java.util.UUID;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {AccountControllerV1.class, FilterChainProxy.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles("Test")
class AccountControllerV1Test {
    @MockBean
    private AccountServiceImpl service;

    @Autowired
    AccountControllerV1 controller;

    @Autowired
    FilterChainProxy springSecurityFilterChain;

    @Autowired
    UserDetailsService userDetailsService;

    private MockMvc mockMvc;

    @BeforeAll
    void setUpBeforeAll() {
        mockMvc = MockMvcBuilders.standaloneSetup(controller)
                .setControllerAdvice(new RestExceptionHandler())
                .apply(SecurityMockMvcConfigurers.springSecurity(springSecurityFilterChain))
                .build();
    }

    private final ObjectMapper objectMapper = new ObjectMapper();
    private static final String HOST = "http://localhost";
    private static final String PORT = ":8081";
    private static final String API = "api/v1/";
    private static final String ACCOUNT_NAME = "Name of account";
    private Account account;
    private AccountDto accountDto;
    private AccountCreateDto accountCreateDto;

    @BeforeEach
    void setUpBeforeEach() {
        String EMAIL_OF_ACCOUNT_OWNER = "m@m.ru";
        String PASSWORD = "UserUser123";
        String NAME_GROUP_OF_ACCOUNT_OWNER = "Name of group of account owner";
        accountCreateDto = AccountCreateDto
                .builder()
                .emailAccountOwner(EMAIL_OF_ACCOUNT_OWNER)
                .passwordAccountOwner(PASSWORD)
                .name(ACCOUNT_NAME)
                .nameGroupAccountOwner(NAME_GROUP_OF_ACCOUNT_OWNER)
                .build();
        account = Account
                .builder()
                .name(ACCOUNT_NAME)
                .build();
        account.setId(UUID.fromString(TestSecurityConfig.ACCOUNT_1_ID));
        accountDto = AccountDto
                .builder()
                .name(ACCOUNT_NAME)
                .build();
        accountDto.setId(UUID.fromString(TestSecurityConfig.ACCOUNT_1_ID));
    }

    @Test
    @WithAnonymousUser
    void create_shouldCreateAccount_whenPassedValidData() throws Exception {
        when(service.create((AccountCreateDto) any())).thenReturn(account);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API + "create-account")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(accountCreateDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.id").value(TestSecurityConfig.ACCOUNT_1_ID))
                .andExpect(jsonPath("$.name").value(ACCOUNT_NAME));
        verify(service, times(1)).create((AccountCreateDto) any());
    }

    @Test
    @WithMockUser(authorities ={"ADMIN", "EXECUTOR", "AUTHOR", "OBSERVER", "ACCOUNT_OWNER"})
    void create_shouldGetForbiddenStatus_whenAuthenticatedUserHasNotAnonymousUser() throws Exception {
        when(service.create((AccountCreateDto) any())).thenReturn(account);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API + "create-account")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(accountCreateDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).create((AccountCreateDto) any());
    }

    @Test
    @WithAnonymousUser
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        accountCreateDto.setName("1");
        accountCreateDto.setNameGroupAccountOwner("");
        accountCreateDto.setEmailAccountOwner("12");
        accountCreateDto.setPasswordAccountOwner("123");
        when(service.create((AccountCreateDto) any())).thenReturn(account);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API + "create-account")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(accountCreateDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.name[?(@.message =~ /%s.*/)]", CommonConstants.SIZE_MUST_BE_BETWEEN).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.nameGroupAccountOwner[?(@.message =~ /%s.*/)]", CommonConstants.SIZE_MUST_BE_BETWEEN)
                        .exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.emailAccountOwner[?(@.message == '%s')]", AAUConstants.INVALID_EMAIL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.passwordAccountOwner[?(@.message == '%s')]", AAUConstants.INVALID_PASSWORD).exists());
        verify(service, times(0)).create((AccountCreateDto) any());
    }

    @Test
    @WithAnonymousUser
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenAllDataIsNull() throws Exception {
        accountCreateDto.setName(null);
        accountCreateDto.setNameGroupAccountOwner(null);
        accountCreateDto.setEmailAccountOwner(null);
        accountCreateDto.setPasswordAccountOwner(null);
        when(service.create((AccountCreateDto) any())).thenReturn(account);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API + "create-account")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(accountCreateDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.name[?(@.message =~ /%s.*/)]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(
                        MockMvcResultMatchers
                                .jsonPath("$.errors.nameGroupAccountOwner[?(@.message =~ /%s.*/)]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.emailAccountOwner[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.passwordAccountOwner[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists());
        verify(service, times(0)).create((AccountCreateDto) any());
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldUpdateAccount_whenPassedValidData() throws Exception {
        accountDto.setDeleted(false);
        accountDto.setVersion(0);
        when(service.update(any())).thenReturn(account);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API + "account")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(accountDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk());
        verify(service, times(1)).update(any());
    }

    @Test
    @WithMockUser(authorities ={"ADMIN", "EXECUTOR", "AUTHOR", "OBSERVER"})
    void update_shouldGetStatusForbidden_whenAuthenticatedUserHasNotRoleAccountOwner() throws Exception {
        accountDto.setDeleted(false);
        accountDto.setVersion(0);
        when(service.update(any())).thenReturn(account);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API + "account")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(accountDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequest_whenInvalidDataPassed() throws Exception {
        when(service.update(any())).thenReturn(account);
        accountDto.setName("");
        accountDto.setVersion(0);
        accountDto.setDeleted(false);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API + "account")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(accountDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.name[?(@.message =~ /%s.*/)]", CommonConstants.SIZE_MUST_BE_BETWEEN).exists());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenAllDataIsNull() throws Exception {
        accountDto.setName(null);
        accountDto.setVersion(null);
        accountDto.setDeleted(null);
        accountDto.setId(null);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API + "account")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(accountDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.id[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.name[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.version[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.deleted[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_1_IS_INNER_GROUP")
    void get_shouldGetAccount_whenPassedValidData() throws Exception {
        when(service.findById(any())).thenReturn(account);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API + "account")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.id").value(TestSecurityConfig.ACCOUNT_1_ID))
                .andExpect(jsonPath("$.name").value(ACCOUNT_NAME));
        verify(service, times(1)).findById(any());
    }

    @Test
    @WithAnonymousUser
    void get_shouldGetStatusForbidden_whenUserIsNotAuthenticated() throws Exception {
        MockHttpServletRequestBuilder request = get(HOST + PORT + API + "account")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).findById(any());
    }
}