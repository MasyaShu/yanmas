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
import static ru.itterminal.botdesk.aau.util.AAUConstants.INVALID_EMAIL;
import static ru.itterminal.botdesk.aau.util.AAUConstants.INVALID_PASSWORD;
import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_NOT_BE_NULL;
import static ru.itterminal.botdesk.commons.util.CommonConstants.SIZE_MUST_BE_BETWEEN;
import static ru.itterminal.botdesk.config.TestSecurityConfig.ACCOUNT_1_ID;

import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.test.context.support.WithAnonymousUser;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers;
import org.springframework.security.web.FilterChainProxy;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.dto.AccountCreateDto;
import ru.itterminal.botdesk.aau.model.dto.AccountDto;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.commons.config.WebTestConfig;
import ru.itterminal.botdesk.config.TestSecurityConfig;

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
        MockitoAnnotations.initMocks(this);
        mockMvc = MockMvcBuilders.standaloneSetup(controller)
                .setControllerAdvice(WebTestConfig.controllerAdvice())
                .apply(SecurityMockMvcConfigurers.springSecurity(springSecurityFilterChain))
                .build();
    }

    private ObjectMapper objectMapper = new ObjectMapper();
    private static String HOST = "http://localhost";
    private static String PORT = ":8081";
    private static String API = "api/v1/";
    private static UUID USER_ID = UUID.fromString("d414bc22-686c-4004-b009-c3569f7914d5");
    private static String ACCOUNT_NAME = "Name of account";
    private static String PASSWORD = "UserUser123";
    private static String NAME_GROUP_OF_ACCOUNT_OWNER = "Name of group of account owner";
    private Account account;
    private AccountDto accountDto;
    private AccountCreateDto accountCreateDto;

    @BeforeEach
    void setUpBeforeEach() {
        String EMAIL_OF_ACCOUNT_OWNER = "m@m.ru";
        accountCreateDto = accountCreateDto
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
        account.setId(UUID.fromString(ACCOUNT_1_ID));
        accountDto = AccountDto
                .builder()
                .name(ACCOUNT_NAME)
                .build();
        accountDto.setId(UUID.fromString(ACCOUNT_1_ID));
    }

    @Test
    @WithAnonymousUser
    public void create_shouldCreateAccount_whenPassedValidData() throws Exception {
        when(service.create((AccountCreateDto) any())).thenReturn(account);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API + "create-account")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(accountCreateDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.id").value(ACCOUNT_1_ID))
                .andExpect(jsonPath("$.name").value(ACCOUNT_NAME));
        verify(service, times(1)).create((AccountCreateDto) any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    public void create_shouldGetForbiddenStatus_whenAuthenticatedUserHasNotRoleAccountOwner() throws Exception {
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
    public void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
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
                .andExpect(jsonPath("$.errors.name[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists())
                .andExpect(jsonPath("$.errors.nameGroupAccountOwner[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN)
                        .exists())
                .andExpect(jsonPath("$.errors.emailAccountOwner[?(@.message == '%s')]", INVALID_EMAIL).exists())
                .andExpect(jsonPath("$.errors.passwordAccountOwner[?(@.message == '%s')]", INVALID_PASSWORD).exists());
        verify(service, times(0)).create((AccountCreateDto) any());
    }

    @Test
    @WithAnonymousUser
    public void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenAllDataIsNull() throws Exception {
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
                .andExpect(jsonPath("$.errors.name[?(@.message =~ /%s.*/)]", MUST_NOT_BE_NULL).exists())
                .andExpect(
                        jsonPath("$.errors.nameGroupAccountOwner[?(@.message =~ /%s.*/)]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.emailAccountOwner[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.passwordAccountOwner[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists());
        verify(service, times(0)).create((AccountCreateDto) any());
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_1_IS_INNER_GROUP")
    public void update_shouldUpdateAccount_whenPassedValidData() throws Exception {
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
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    public void update_shouldGetStatusForbidden_whenAuthenticatedUserHasNotRoleAccountOwner() throws Exception {
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
    public void update_shouldGetStatusBadRequest_whenInvalidDataPassed() throws Exception {
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
                .andExpect(jsonPath("$.errors.name[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_1_IS_INNER_GROUP")
    public void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenAllDataIsNull() throws Exception {
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
                .andExpect(jsonPath("$.errors.id[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.name[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.version[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.deleted[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_1_IS_INNER_GROUP")
    public void get_shouldGetAccount_whenPassedValidData() throws Exception {
        when(service.findById(any())).thenReturn(account);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API + "account")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.id").value(ACCOUNT_1_ID))
                .andExpect(jsonPath("$.name").value(ACCOUNT_NAME));
        verify(service, times(1)).findById(any());
    }

    @Test
    @WithAnonymousUser
    public void get_shouldGetStatusForbidden_whenUserIsNotAuthenticated() throws Exception {
        MockHttpServletRequestBuilder request = get(HOST + PORT + API + "account")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).findById(any());
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_1_IS_INNER_GROUP")
    void physicalDelete_shouldThrowUnsupportedOperationException_untilMethodWouldBeImplemented() throws Exception {
        accountDto.setDeleted(false);
        accountDto.setVersion(0);
        MockHttpServletRequestBuilder request = delete(HOST + PORT + API + "account")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(accountDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isMethodNotAllowed());
    }

    @Test
    @WithAnonymousUser
    void physicalDelete_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        mockMvc.perform(delete(HOST + PORT + API + "account"))
                .andDo(print())
                .andExpect(status().isForbidden());
    }
}