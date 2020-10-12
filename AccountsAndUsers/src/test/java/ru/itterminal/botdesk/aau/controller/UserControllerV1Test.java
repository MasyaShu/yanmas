package ru.itterminal.botdesk.aau.controller;

import static org.hamcrest.Matchers.hasSize;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
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
import static ru.itterminal.botdesk.aau.util.AAUConstants.MUST_BE_ANY_OF_EN_RU;
import static ru.itterminal.botdesk.aau.util.AAUConstants.MUST_BE_ANY_OF_FIRST_NAME_SECOND_NAME;
import static ru.itterminal.botdesk.commons.controller.BaseController.PAGE_DEFAULT_VALUE;
import static ru.itterminal.botdesk.commons.controller.BaseController.SIZE_DEFAULT_VALUE;
import static ru.itterminal.botdesk.commons.util.CommonConstants.DELETED_ASSERT_FALSE;
import static ru.itterminal.botdesk.commons.util.CommonConstants.MESSAGE_NOT_READABLE;
import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_BE_ANY_OF_ALL_TRUE_FALSE;
import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_BE_ANY_OF_ASC_DESC;
import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_BE_GREATER_THAN_OR_EQUAL_TO_0;
import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY;
import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_NOT_BE_EMPTY;
import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_NOT_BE_NULL;
import static ru.itterminal.botdesk.commons.util.CommonConstants.REQUEST_NOT_READABLE;
import static ru.itterminal.botdesk.commons.util.CommonConstants.SIZE_MUST_BE_BETWEEN;
import static ru.itterminal.botdesk.config.TestSecurityConfig.*;

import java.util.Collections;
import java.util.List;
import java.util.Set;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.MediaType;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetailsService;
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
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.Language;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.dto.UserDto;
import ru.itterminal.botdesk.aau.model.dto.UserFilterDto;
import ru.itterminal.botdesk.aau.model.spec.UserSpec;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.config.TestSecurityConfig;
import ru.itterminal.botdesk.jwt.JwtUser;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {UserControllerV1.class, UserSpec.class, FilterChainProxy.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles("Test")
class UserControllerV1Test {

    @MockBean
    private UserServiceImpl service;

    @Autowired
    private UserControllerV1 controller;

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
    private static String API = "api/v1/user/";
    private static String PASSWORD_1 = "UserUser123";
    private static String PASSWORD_2 = "UserUser321";
    private static String FIRST_NAME = "Ivan";
    private static String SECOND_NAME = "Ivanov";
    private static String PHONE = "123456";
    private static String INVALID_FIRST_NAME = "123456789012345678901";
    private static String INVALID_SECOND_NAME = "1234567890123456789012345678901";
    private static String INVALID_PHONE = "1234567890123456789012345678901";
    private static String INVALID_SORT_BY = "ERROR";
    private static String INVALID_DELETED = "ERROR";
    private static String INVALID_LANGUAGE = "ge";
    private static String INVALID_DIRECTION = "ERROR";

    private static String INVALID_EMAIL_1 = "it-terminal_mail.ru"; //absent @
    private static String INVALID_EMAIL_2 = "it-terminal_@mail.рф"; //characters are not allowed
    private static String INVALID_EMAIL_3 = "it-terminal_@.ru"; //missing domain name
    private static String INVALID_EMAIL_4 = "it-terminal_@mail"; //missing domain name
    private static String INVALID_EMAIL_5 = "it-terminal_@@mail.ru"; //two @
    private static String INVALID_EMAIL_6 = "@mail.ru"; //missing name
    private static String INVALID_EMAIL_7 = "it terminal@mail.ru"; //space in name
    private static String INVALID_EMAIL_8 = "it-terminal@ mail.ru"; //space in domain
    //private static String INVALID_EMAIL_9 = "it-terminal@mail.1"; //domain name are not allowed

    private Set<String> invalidEmail = Set.of(INVALID_EMAIL_1, INVALID_EMAIL_2, INVALID_EMAIL_3, INVALID_EMAIL_4,
            INVALID_EMAIL_5, INVALID_EMAIL_6, INVALID_EMAIL_7, INVALID_EMAIL_8);

    private static String INVALID_PASSWORD_1 = "Itt12"; //Valid characters, not valid length (5)
    private static String INVALID_PASSWORD_2 = "itt12"; //not valid characters, not valid length (5)
    private static String INVALID_PASSWORD_3 = "itt123"; //valid length (6), not valid characters
    private static String INVALID_PASSWORD_4 = "Itt12Itt12Itt12Itt12Itt12"; //Valid characters, not valid length (25)
    private static String INVALID_PASSWORD_5 = "IttItt"; //no numbers
    private static String INVALID_PASSWORD_6 = "ittitt2"; //no upcase letter
    private static String INVALID_PASSWORD_7 = "ITTITT2"; //no lowercase letter
    private static String INVALID_PASSWORD_8 = "IttItt 2"; //space
    private static String INVALID_PASSWORD_9 = "123123"; //numbers

    private Set<String> invalidPassword =
            Set.of(INVALID_PASSWORD_1, INVALID_PASSWORD_2, INVALID_PASSWORD_3, INVALID_PASSWORD_4,
                    INVALID_PASSWORD_5, INVALID_PASSWORD_6, INVALID_PASSWORD_7, INVALID_PASSWORD_8, INVALID_PASSWORD_9);

    private User user_1;
    private User user_2;
    private Account account_1;
    private Group group_1;
    private Role roleAdmin = new Role(Roles.ADMIN.toString());
    private Role roleSuperAdmin = new Role(Roles.SUPER_ADMIN.toString());
    private Set<Role> roles_1 = Set.of(roleAdmin, roleSuperAdmin);
    private UserDto userDto;
    private UserFilterDto userFilterDto;

    @BeforeEach
    void setUpBeforeEach() {
        account_1 = new Account().builder()
                .language(Language.RU.toString())
                .build();
        account_1.setId(UUID.fromString(ACCOUNT_1_ID));
        group_1 = new Group();
        group_1.setId(UUID.fromString(GROUP_1_ID));
        user_1 = new User().builder()
                .email(EMAIL_1)
                .password(PASSWORD_1)
                .account(account_1)
                .group(group_1)
                .isArchived(false)
                .roles(roles_1)
                .build();
        user_1.setId(UUID.fromString(USER_1_ID));
        user_2 = new User().builder()
                .email(EMAIL_2)
                .password(PASSWORD_2)
                .account(account_1)
                .group(group_1)
                .isArchived(false)
                .roles(roles_1)
                .build();
        user_2.setId(UUID.fromString(USER_2_ID));
        userDto = new UserDto().builder()
                .email(EMAIL_1)
                .password(PASSWORD_1)
                .account(account_1)
                .group(group_1)
                .isArchived(false)
                .roles(roles_1)
                .build();
        userDto.setDeleted(false);
        userFilterDto = new UserFilterDto();
        userFilterDto.setEmail(EMAIL_1);
        userFilterDto.setFirstName(FIRST_NAME);
        userFilterDto.setSecondName(SECOND_NAME);
        userFilterDto.setPhone(PHONE);
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    public void create_shouldCreate_whenValidDataPassed() throws Exception {
        when(service.create(any())).thenReturn(user_1);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.id").value(USER_1_ID))
                .andExpect(jsonPath("$.email").value(EMAIL_1))
                .andExpect(jsonPath("$.password").doesNotExist());
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    public void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        userDto.setEmail(INVALID_EMAIL_1);
        userDto.setDeleted(true);
        userDto.setPassword("");
        userDto.setGroup(null);
        userDto.setAccount(null);
        userDto.setRoles(Collections.emptySet());
        userDto.setFirstName(INVALID_FIRST_NAME);
        userDto.setSecondName(INVALID_SECOND_NAME);
        userDto.setPhone(INVALID_PHONE);
        userDto.setLanguage(INVALID_LANGUAGE);
        userDto.setIsArchived(null);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errors.firstName[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists())
                .andExpect(jsonPath("$.errors.secondName[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists())
                .andExpect(jsonPath("$.errors.phone[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists())
                .andExpect(jsonPath("$.errors.deleted[?(@.message == '%s')]", DELETED_ASSERT_FALSE).exists())
                .andExpect(jsonPath("$.errors.isArchived[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.roles[?(@.message == '%s')]", MUST_NOT_BE_EMPTY).exists())
                .andExpect(jsonPath("$.errors.language[?(@.message == '%s')]", MUST_BE_ANY_OF_EN_RU).exists())
                .andExpect(jsonPath("$.errors.email[?(@.message == '%s')]", INVALID_EMAIL).exists())
                .andExpect(jsonPath("$.errors.account[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.group[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.password[?(@.message == '%s')]", INVALID_PASSWORD).exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    public void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenAllPassedDataIsNull() throws Exception {
        userDto.setEmail(null);
        userDto.setDeleted(null);
        userDto.setPassword(null);
        userDto.setGroup(null);
        userDto.setAccount(null);
        userDto.setRoles(null);
        userDto.setFirstName(null);
        userDto.setSecondName(null);
        userDto.setPhone(null);
        userDto.setLanguage(null);
        userDto.setIsArchived(null);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errors.deleted[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.isArchived[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.roles[?(@.message == '%s')]", MUST_NOT_BE_EMPTY).exists())
                .andExpect(jsonPath("$.errors.email[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.account[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.group[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.password[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    public void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenIdAndVersionNotNull() throws Exception {
        userDto.setId(UUID.randomUUID());
        userDto.setVersion(15);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errors.id[?(@.message == '%s')]", MUST_BE_NULL_FOR_THE_NEW_ENTITY).exists())
                .andExpect(
                        jsonPath("$.errors.version[?(@.message == '%s')]", MUST_BE_NULL_FOR_THE_NEW_ENTITY).exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    public void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidEmailPassed() throws Exception {
        for (String ie : invalidEmail) {
            userDto.setEmail(ie);
            MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                    .contentType(MediaType.APPLICATION_JSON)
                    .accept(MediaType.APPLICATION_JSON)
                    .content(objectMapper.writeValueAsString(userDto));
            mockMvc.perform(request)
                    .andDo(print())
                    .andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.errors.email[?(@.message == '%s')]", INVALID_EMAIL).exists());
            verify(service, times(0)).create(any());
        }
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    public void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidPasswordPassed() throws Exception {
        for (String ip : invalidPassword) {
            userDto.setPassword(ip);
            MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                    .contentType(MediaType.APPLICATION_JSON)
                    .accept(MediaType.APPLICATION_JSON)
                    .content(objectMapper.writeValueAsString(userDto));
            mockMvc.perform(request)
                    .andDo(print())
                    .andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.errors.password[?(@.message == '%s')]", INVALID_PASSWORD).exists());
            verify(service, times(0)).create(any());
        }
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    public void update_shouldUpdate_whenValidDataPassed() throws Exception {
        userDto.setId(UUID.fromString(USER_1_ID));
        userDto.setVersion(1);
        when(service.update(any())).thenReturn(user_1);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.id").value(USER_1_ID))
                .andExpect(jsonPath("$.email").value(EMAIL_1))
                .andExpect(jsonPath("$.password").doesNotExist());
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    public void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        userDto.setEmail(INVALID_EMAIL_1);
        userDto.setDeleted(null);
        userDto.setPassword("");
        userDto.setGroup(null);
        userDto.setAccount(null);
        userDto.setRoles(Collections.emptySet());
        userDto.setFirstName("123456789012345678901");
        userDto.setSecondName("1234567890123456789012345678901");
        userDto.setPhone("1234567890123456789012345678901");
        userDto.setLanguage("gr");
        userDto.setIsArchived(null);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errors.id[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.version[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.firstName[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists())
                .andExpect(jsonPath("$.errors.secondName[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists())
                .andExpect(jsonPath("$.errors.phone[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists())
                .andExpect(jsonPath("$.errors.deleted[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.isArchived[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.roles[?(@.message == '%s')]", MUST_NOT_BE_EMPTY).exists())
                .andExpect(jsonPath("$.errors.language[?(@.message == '%s')]", MUST_BE_ANY_OF_EN_RU).exists())
                .andExpect(jsonPath("$.errors.email[?(@.message == '%s')]", INVALID_EMAIL).exists())
                .andExpect(jsonPath("$.errors.account[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.group[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.password[?(@.message == '%s')]", INVALID_PASSWORD).exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    public void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenAllPassedDataIsNull() throws Exception {
        userDto.setEmail(null);
        userDto.setDeleted(null);
        userDto.setPassword(null);
        userDto.setGroup(null);
        userDto.setAccount(null);
        userDto.setRoles(null);
        userDto.setFirstName(null);
        userDto.setSecondName(null);
        userDto.setPhone(null);
        userDto.setLanguage(null);
        userDto.setIsArchived(null);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errors.id[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.version[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.deleted[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.isArchived[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.roles[?(@.message == '%s')]", MUST_NOT_BE_EMPTY).exists())
                .andExpect(jsonPath("$.errors.email[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.account[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.group[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.password[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    public void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenVersionIsNegative() throws Exception {
        userDto.setVersion(-15);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(
                        jsonPath("$.errors.version[?(@.message == '%s')]", MUST_BE_GREATER_THAN_OR_EQUAL_TO_0)
                                .exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    public void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidIdPassed() throws Exception {
        userDto.setId(UUID.fromString(USER_1_ID));
        userDto.setVersion(1);
        String json = objectMapper.writeValueAsString(userDto);
        json = json.replace(USER_1_ID, "abracadabra");
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(json);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.title").value(MESSAGE_NOT_READABLE));
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    public void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidEmailPassed() throws Exception {
        for (String ie : invalidEmail) {
            userDto.setEmail(ie);
            MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                    .contentType(MediaType.APPLICATION_JSON)
                    .accept(MediaType.APPLICATION_JSON)
                    .content(objectMapper.writeValueAsString(userDto));
            mockMvc.perform(request)
                    .andDo(print())
                    .andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.errors.email[?(@.message == '%s')]", INVALID_EMAIL).exists());
            verify(service, times(0)).create(any());
        }
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    public void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidPasswordPassed() throws Exception {
        for (String ip : invalidPassword) {
            userDto.setPassword(ip);
            MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                    .contentType(MediaType.APPLICATION_JSON)
                    .accept(MediaType.APPLICATION_JSON)
                    .content(objectMapper.writeValueAsString(userDto));
            mockMvc.perform(request)
                    .andDo(print())
                    .andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.errors.password[?(@.message == '%s')]", INVALID_PASSWORD).exists());
            verify(service, times(0)).create(any());
        }
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    public void getById_shouldFindOneUser_whenUserExistInDatabaseByPassedId() throws Exception {
        when(service.findByIdAndAccountId(any(), any())).thenReturn(user_1);
        mockMvc.perform(get(HOST + PORT + API + USER_1_ID))
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.email").value(EMAIL_1))
                .andExpect(jsonPath("$.id").value(USER_1_ID));
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    void getById_shouldRespondNotFound_whenPassedIdNotExist() throws Exception {
        when(service.findByIdAndAccountId(any(), any())).thenThrow(EntityNotExistException.class);
        mockMvc.perform(get(HOST + PORT + API + USER_1_ID))
                .andDo(print())
                .andExpect(status().isNotFound());
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    public void getById_shouldGetStatusBadRequest_whenUiidIsInvalid() throws Exception {
        mockMvc.perform(get(HOST + PORT + API + "Abracadabra"))
                .andDo(print())
                .andExpect(status().isBadRequest());
        verifyZeroInteractions(service);
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    public void getByFilter_shouldFindTwoUsers_whenUsersExistInDatabaseByPassedFilter() throws Exception {
        Pageable pageable =
                PageRequest.of(Integer.parseInt(PAGE_DEFAULT_VALUE), Integer.parseInt(SIZE_DEFAULT_VALUE),
                        Sort.by("firstName").ascending());
        Page<User> userPageExpected = new PageImpl<User>(List.of(user_1, user_2), pageable, 2);
        when(service.findAllByFilter(any(), any())).thenReturn(userPageExpected);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].id").value(USER_1_ID))
                .andExpect(jsonPath("$.content[1].id").value(USER_2_ID))
                .andExpect(jsonPath("$.content[0].email").value(EMAIL_1))
                .andExpect(jsonPath("$.content[1].email").value(EMAIL_2))
                .andExpect(jsonPath("$.content[0].password").doesNotExist())
                .andExpect(jsonPath("$.content[1].password").doesNotExist())
                .andExpect(jsonPath("$.content", hasSize(2)));
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    public void getByFilter_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        userFilterDto.setEmail(INVALID_EMAIL_1);
        userFilterDto.setFirstName(INVALID_FIRST_NAME);
        userFilterDto.setSecondName(INVALID_SECOND_NAME);
        userFilterDto.setPhone(INVALID_PHONE);
        userFilterDto.setSortBy(INVALID_SORT_BY);
        userFilterDto.setDeleted(INVALID_DELETED);
        userFilterDto.setDirection(INVALID_DIRECTION);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errors.firstName[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists())
                .andExpect(jsonPath("$.errors.secondName[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists())
                .andExpect(jsonPath("$.errors.email[?(@.message == '%s')]", INVALID_EMAIL).exists())
                .andExpect(jsonPath("$.errors.deleted[?(@.message == '%s')]", MUST_BE_ANY_OF_ALL_TRUE_FALSE).exists())
                .andExpect(jsonPath("$.errors.direction[?(@.message == '%s')]", MUST_BE_ANY_OF_ASC_DESC).exists())
                .andExpect(jsonPath("$.errors.sortBy[?(@.message == '%s')]", MUST_BE_ANY_OF_FIRST_NAME_SECOND_NAME)
                        .exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    public void getByFilter_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidSizeAndPagePassed()
            throws Exception {
        MockHttpServletRequestBuilder request = get(HOST + PORT + API + "?page=-1&size=0")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.title").value(REQUEST_NOT_READABLE));
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    public void getByFilter_shouldGetStatusBadRequestWithErrorsDescriptions_whenFilterIsEmpty() throws Exception {
        userFilterDto.setEmail("");
        userFilterDto.setFirstName("");
        userFilterDto.setSecondName("");
        userFilterDto.setPhone("");
        userFilterDto.setSortBy("");
        userFilterDto.setDeleted("");
        userFilterDto.setDirection("");
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errors.firstName[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists())
                .andExpect(jsonPath("$.errors.secondName[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists())
                .andExpect(jsonPath("$.errors.phone[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists())
                .andExpect(jsonPath("$.errors.email[?(@.message == '%s')]", INVALID_EMAIL).exists())
                .andExpect(jsonPath("$.errors.deleted[?(@.message == '%s')]", MUST_BE_ANY_OF_ALL_TRUE_FALSE).exists())
                .andExpect(jsonPath("$.errors.direction[?(@.message == '%s')]", MUST_BE_ANY_OF_ASC_DESC).exists())
                .andExpect(jsonPath("$.errors.sortBy[?(@.message == '%s')]", MUST_BE_ANY_OF_FIRST_NAME_SECOND_NAME)
                        .exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    public void getByFilter_shouldFindTwoUsers_whenFilterIsNew() throws Exception {
        UserFilterDto userFilterDto = new UserFilterDto();
        Pageable pageable =
                PageRequest.of(Integer.parseInt(PAGE_DEFAULT_VALUE), Integer.parseInt(SIZE_DEFAULT_VALUE),
                        Sort.by("firstName").ascending());
        Page<User> userPageExpected = new PageImpl<User>(List.of(user_1, user_2), pageable, 2);
        when(service.findAllByFilter(any(), any())).thenReturn(userPageExpected);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].id").value(USER_1_ID))
                .andExpect(jsonPath("$.content[1].id").value(USER_2_ID))
                .andExpect(jsonPath("$.content", hasSize(2)));
        verify(service, times(1)).findAllByFilter(any(), any());
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    public void getByFilter_shouldFindTwoUsers_whenDefaultFieldsInFilterIsNull() throws Exception {
        userFilterDto.setSortBy(null);
        userFilterDto.setDeleted(null);
        userFilterDto.setDirection(null);
        Pageable pageable =
                PageRequest.of(Integer.parseInt(PAGE_DEFAULT_VALUE), Integer.parseInt(SIZE_DEFAULT_VALUE),
                        Sort.by("firstName").ascending());
        Page<User> userPageExpected = new PageImpl<User>(List.of(user_1, user_2), pageable, 2);
        when(service.findAllByFilter(any(), any())).thenReturn(userPageExpected);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].id").value(USER_1_ID))
                .andExpect(jsonPath("$.content[1].id").value(USER_2_ID))
                .andExpect(jsonPath("$.content", hasSize(2)));
        verify(service, times(1)).findAllByFilter(any(), any());
    }

    @Test
    @WithUserDetails(value = "ADMIN_ACCOUNT_1")
    void physicalDelete_shouldThrowUnsupportedOperationException_untilMethodWouldBeImplemented() throws Exception {
        mockMvc.perform(delete(HOST + PORT + API + USER_1_ID))
                .andDo(print())
                .andExpect(status().isMethodNotAllowed());
    }
}