package ru.itterminal.botdesk.aau.controller;

import static org.hamcrest.Matchers.hasSize;
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
import static ru.itterminal.botdesk.aau.model.Roles.ACCOUNT_OWNER;
import static ru.itterminal.botdesk.aau.model.Roles.ADMIN;
import static ru.itterminal.botdesk.aau.util.AAUConstants.INVALID_EMAIL;
import static ru.itterminal.botdesk.aau.util.AAUConstants.INVALID_PASSWORD;
import static ru.itterminal.botdesk.aau.util.AAUConstants.MUST_BE_ANY_OF_FIRST_NAME_SECOND_NAME;
import static ru.itterminal.botdesk.commons.controller.BaseController.CHECK_ACCESS;
import static ru.itterminal.botdesk.commons.controller.BaseController.PAGE_DEFAULT_VALUE;
import static ru.itterminal.botdesk.commons.controller.BaseController.SIZE_DEFAULT_VALUE;
import static ru.itterminal.botdesk.commons.util.CommonConstants.MESSAGE_NOT_READABLE;
import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_BE_ANY_OF_ALL_TRUE_FALSE;
import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_BE_ANY_OF_ASC_DESC;
import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_BE_GREATER_THAN_OR_EQUAL_TO_0;
import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY;
import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_NOT_BE_NULL;
import static ru.itterminal.botdesk.commons.util.CommonConstants.REQUEST_NOT_READABLE;
import static ru.itterminal.botdesk.commons.util.CommonConstants.SIZE_MUST_BE_BETWEEN;
import static ru.itterminal.botdesk.config.TestSecurityConfig.ACCOUNT_1_ID;
import static ru.itterminal.botdesk.config.TestSecurityConfig.EMAIL_1;
import static ru.itterminal.botdesk.config.TestSecurityConfig.EMAIL_2;
import static ru.itterminal.botdesk.config.TestSecurityConfig.GROUP_1_ID;

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
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.dto.UserDto;
import ru.itterminal.botdesk.aau.model.dto.UserFilterDto;
import ru.itterminal.botdesk.aau.model.spec.UserSpec;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.RoleServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.exception.RestExceptionHandler;
import ru.itterminal.botdesk.config.TestSecurityConfig;

@SuppressWarnings("ALL")
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {UserControllerV1.class, UserSpec.class, FilterChainProxy.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles("Test")
class UserControllerV1Test {

    @MockBean
    private UserServiceImpl service;

    @MockBean
    private AccountServiceImpl accountService;

    @MockBean
    private RoleServiceImpl roleService;

    @MockBean
    private GroupServiceImpl groupService;

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
                .setControllerAdvice(new RestExceptionHandler())
                .apply(SecurityMockMvcConfigurers.springSecurity(springSecurityFilterChain))
                .build();
    }

    private final ObjectMapper objectMapper = new ObjectMapper();
    private static final String HOST = "http://localhost";
    private static final String PORT = ":8081";
    private static final String API = "api/v1/user/";
    public static final String USER_1_ID = "d592facb-e6ee-4801-8310-9c7708eb6e6c";
    public static final String USER_2_ID = "86840939-c488-448b-a473-cd9e1097dd32";
    public static final String ROLE_ADMIN_ID = "d7597321-239f-4e06-84a6-853e71574896";
    public static final String ROLE_ACCOUNT_OWNER_ID = "c4edeb4d-0b1e-4c8f-99f6-9534389408a0";
    private static final String INVALID_FIRST_NAME = "123456789012345678901";
    private static final String INVALID_SECOND_NAME = "1234567890123456789012345678901";
    private static final String INVALID_PHONE = "1234567890123456789012345678901";

    private static final String INVALID_EMAIL_1 = "it-terminal_mail.ru"; //absent @
    private static final String INVALID_EMAIL_2 = "it-terminal_@mail.рф"; //characters are not allowed
    private static final String INVALID_EMAIL_3 = "it-terminal_@.ru"; //missing domain name
    private static final String INVALID_EMAIL_4 = "it-terminal_@mail"; //missing domain name
    private static final String INVALID_EMAIL_5 = "it-terminal_@@mail.ru"; //two @
    private static final String INVALID_EMAIL_6 = "@mail.ru"; //missing name
    private static final String INVALID_EMAIL_7 = "it terminal@mail.ru"; //space in name
    private static final String INVALID_EMAIL_8 = "it-terminal@ mail.ru"; //space in domain

    private final Set<String> invalidEmail = Set.of(INVALID_EMAIL_1, INVALID_EMAIL_2, INVALID_EMAIL_3, INVALID_EMAIL_4,
            INVALID_EMAIL_5, INVALID_EMAIL_6, INVALID_EMAIL_7, INVALID_EMAIL_8);

    private static final String INVALID_PASSWORD_1 = "Itt12"; //Valid characters, not valid length (5)
    private static final String INVALID_PASSWORD_2 = "itt12"; //not valid characters, not valid length (5)
    private static final String INVALID_PASSWORD_3 = "itt123"; //valid length (6), not valid characters
    private static final String INVALID_PASSWORD_4 = "Itt12Itt12Itt12Itt12Itt12"; //Valid characters, not valid
    // length (25)
    private static final String INVALID_PASSWORD_5 = "IttItt"; //no numbers
    private static final String INVALID_PASSWORD_6 = "ittitt2"; //no upcase letter
    private static final String INVALID_PASSWORD_7 = "ITTITT2"; //no lowercase letter
    private static final String INVALID_PASSWORD_8 = "IttItt 2"; //space
    private static final String INVALID_PASSWORD_9 = "123123"; //numbers

    private final Set<String> invalidPassword =
            Set.of(INVALID_PASSWORD_1, INVALID_PASSWORD_2, INVALID_PASSWORD_3, INVALID_PASSWORD_4,
                    INVALID_PASSWORD_5, INVALID_PASSWORD_6, INVALID_PASSWORD_7, INVALID_PASSWORD_8, INVALID_PASSWORD_9);

    private User user_1;
    private User user_2;
    private Account account_1;
    private Group group_1;
    private final Role roleAdmin = new Role(ADMIN.toString(), ADMIN.getWeight());
    private final Role roleAccountOwner = new Role(ACCOUNT_OWNER.toString(), ACCOUNT_OWNER.getWeight());
    private UserDto userDtoFromAccount_1;
    private UserFilterDto userFilterDto;

    @BeforeEach
    void setUpBeforeEach() {
        roleAdmin.setId(UUID.fromString(ROLE_ADMIN_ID));
        roleAccountOwner.setId(UUID.fromString(ROLE_ACCOUNT_OWNER_ID));
        account_1 = new Account();
        account_1.setId(UUID.fromString(ACCOUNT_1_ID));
        group_1 = Group
                .builder()
                .isInner(true)
                .build();
        group_1.setId(UUID.fromString(GROUP_1_ID));
        String PASSWORD_1 = "UserUser123";
        user_1 = User
                .builder()
                .email(EMAIL_1)
                .password(PASSWORD_1)
                .account(account_1)
                .ownGroup(group_1)
                .isArchived(false)
                .role(roleAdmin)
                .build();
        user_1.setId(UUID.fromString(USER_1_ID));
        String PASSWORD_2 = "UserUser321";
        user_2 = User
                .builder()
                .email(EMAIL_2)
                .password(PASSWORD_2)
                .account(account_1)
                .ownGroup(group_1)
                .isArchived(false)
                .role(roleAccountOwner)
                .build();
        user_2.setId(UUID.fromString(USER_2_ID));
        userDtoFromAccount_1 = UserDto
                .builder()
                .email(EMAIL_1)
                .password(PASSWORD_1)
                .groupId(group_1.getId())
                .roleId(roleAdmin.getId())
                .build();
        userDtoFromAccount_1.setDeleted(false);
        userFilterDto = new UserFilterDto();
        userFilterDto.setEmail(EMAIL_1);
        String FIRST_NAME = "Ivan";
        userFilterDto.setFirstName(FIRST_NAME);
        String SECOND_NAME = "Ivanov";
        userFilterDto.setSecondName(SECOND_NAME);
        String PHONE = "123456";
        userFilterDto.setPhone(PHONE);
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldCreate_whenValidDataPassed() throws Exception {
        userDtoFromAccount_1.setDeleted(null);
        when(service.create(any())).thenReturn(user_1);
        when(accountService.findById(any())).thenReturn(account_1);
        when(roleService.findById(any())).thenReturn(roleAdmin);
        when(groupService.findById(any())).thenReturn(group_1);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.id").value(USER_1_ID))
                .andExpect(jsonPath("$.email").value(EMAIL_1))
                .andExpect(jsonPath("$.password").doesNotExist());
    }

    @Test
    @WithAnonymousUser
    void create_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusForbidden_whenNotAllowedRole() throws Exception {
        userDtoFromAccount_1.setDeleted(null);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        userDtoFromAccount_1.setEmail(INVALID_EMAIL_1);
        userDtoFromAccount_1.setDeleted(true);
        userDtoFromAccount_1.setPassword("");
        userDtoFromAccount_1.setGroupId(null);
        userDtoFromAccount_1.setRoleId(null);
        userDtoFromAccount_1.setFirstName(INVALID_FIRST_NAME);
        userDtoFromAccount_1.setSecondName(INVALID_SECOND_NAME);
        userDtoFromAccount_1.setPhone(INVALID_PHONE);
        userDtoFromAccount_1.setIsArchived(false);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errors.firstName[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists())
                .andExpect(jsonPath("$.errors.secondName[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists())
                .andExpect(jsonPath("$.errors.phone[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists())
                .andExpect(jsonPath("$.errors.deleted[?(@.message == '%s')]", MUST_BE_NULL_FOR_THE_NEW_ENTITY).exists())
                .andExpect(
                        jsonPath("$.errors.isArchived[?(@.message == '%s')]", MUST_BE_NULL_FOR_THE_NEW_ENTITY).exists())
                .andExpect(jsonPath("$.errors.roleId[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.email[?(@.message == '%s')]", INVALID_EMAIL).exists())
                .andExpect(jsonPath("$.errors.groupId[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.password[?(@.message == '%s')]", INVALID_PASSWORD).exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenAllPassedDataIsNull() throws Exception {
        userDtoFromAccount_1.setEmail(null);
        userDtoFromAccount_1.setDeleted(null);
        userDtoFromAccount_1.setPassword(null);
        userDtoFromAccount_1.setGroupId(null);
        userDtoFromAccount_1.setRoleId(null);
        userDtoFromAccount_1.setFirstName(null);
        userDtoFromAccount_1.setSecondName(null);
        userDtoFromAccount_1.setPhone(null);
        userDtoFromAccount_1.setIsArchived(false);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(
                        jsonPath("$.errors.isArchived[?(@.message == '%s')]", MUST_BE_NULL_FOR_THE_NEW_ENTITY).exists())
                .andExpect(jsonPath("$.errors.roleId[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.email[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.groupId[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.password[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenVersionNotNull() throws Exception {
        userDtoFromAccount_1.setId(UUID.randomUUID());
        userDtoFromAccount_1.setVersion(15);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errors.id[?(@.message == '%s')]", MUST_BE_NULL_FOR_THE_NEW_ENTITY).exists())
                .andExpect(
                        jsonPath("$.errors.version[?(@.message == '%s')]", MUST_BE_NULL_FOR_THE_NEW_ENTITY).exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidEmailPassed() throws Exception {
        for (String ie : invalidEmail) {
            userDtoFromAccount_1.setEmail(ie);
            MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                    .contentType(MediaType.APPLICATION_JSON)
                    .accept(MediaType.APPLICATION_JSON)
                    .content(objectMapper.writeValueAsString(userDtoFromAccount_1));
            mockMvc.perform(request)
                    .andDo(print())
                    .andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.errors.email[?(@.message == '%s')]", INVALID_EMAIL).exists());
            verify(service, times(0)).create(any());
        }
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidPasswordPassed() throws Exception {
        for (String ip : invalidPassword) {
            userDtoFromAccount_1.setPassword(ip);
            MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                    .contentType(MediaType.APPLICATION_JSON)
                    .accept(MediaType.APPLICATION_JSON)
                    .content(objectMapper.writeValueAsString(userDtoFromAccount_1));
            mockMvc.perform(request)
                    .andDo(print())
                    .andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.errors.password[?(@.message == '%s')]", INVALID_PASSWORD).exists());
            verify(service, times(0)).create(any());
        }
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldUpdate_whenValidDataPassed() throws Exception {
        userDtoFromAccount_1.setId(UUID.fromString(USER_1_ID));
        userDtoFromAccount_1.setVersion(1);
        userDtoFromAccount_1.setIsArchived(false);
        when(service.update(any())).thenReturn(user_1);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.id").value(USER_1_ID))
                .andExpect(jsonPath("$.email").value(EMAIL_1))
                .andExpect(jsonPath("$.password").doesNotExist());
        verify(service, times(1)).update(any());
    }

    @Test
    @WithAnonymousUser
    void update_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusForbidden_whenNotAllowedRole() throws Exception {
        userDtoFromAccount_1.setId(UUID.fromString(USER_1_ID));
        userDtoFromAccount_1.setVersion(0);
        userDtoFromAccount_1.setIsArchived(false);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        userDtoFromAccount_1.setEmail(INVALID_EMAIL_1);
        userDtoFromAccount_1.setDeleted(null);
        userDtoFromAccount_1.setPassword("");
        userDtoFromAccount_1.setGroupId(null);
        userDtoFromAccount_1.setRoleId(null);
        userDtoFromAccount_1.setFirstName("123456789012345678901");
        userDtoFromAccount_1.setSecondName("1234567890123456789012345678901");
        userDtoFromAccount_1.setPhone("1234567890123456789012345678901");
        userDtoFromAccount_1.setIsArchived(null);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoFromAccount_1));
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
                .andExpect(jsonPath("$.errors.roleId[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.email[?(@.message == '%s')]", INVALID_EMAIL).exists())
                .andExpect(jsonPath("$.errors.groupId[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.password[?(@.message == '%s')]", INVALID_PASSWORD).exists());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenAllPassedDataIsNull() throws Exception {
        userDtoFromAccount_1.setEmail(null);
        userDtoFromAccount_1.setDeleted(null);
        userDtoFromAccount_1.setPassword(null);
        userDtoFromAccount_1.setGroupId(null);
        userDtoFromAccount_1.setRoleId(null);
        userDtoFromAccount_1.setFirstName(null);
        userDtoFromAccount_1.setSecondName(null);
        userDtoFromAccount_1.setPhone(null);
        userDtoFromAccount_1.setIsArchived(null);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errors.id[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.version[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.deleted[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.isArchived[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.roleId[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.email[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.groupId[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenAllPassedDataIsEpmty() throws Exception {
        userDtoFromAccount_1.setEmail("");
        userDtoFromAccount_1.setPassword("");
        userDtoFromAccount_1.setFirstName("");
        userDtoFromAccount_1.setSecondName("");
        userDtoFromAccount_1.setPhone("");
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errors.id[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.email[?(@.message == '%s')]", INVALID_EMAIL).exists())
                .andExpect(jsonPath("$.errors.firstName[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists())
                .andExpect(jsonPath("$.errors.secondName[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists())
                .andExpect(jsonPath("$.errors.phone[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenVersionIsNegative() throws Exception {
        userDtoFromAccount_1.setVersion(-15);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(
                        jsonPath("$.errors.version[?(@.message == '%s')]", MUST_BE_GREATER_THAN_OR_EQUAL_TO_0)
                                .exists());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidIdPassed() throws Exception {
        userDtoFromAccount_1.setId(UUID.fromString(USER_1_ID));
        userDtoFromAccount_1.setVersion(1);
        String json = objectMapper.writeValueAsString(userDtoFromAccount_1);
        json = json.replace(USER_1_ID, "abracadabra");
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(json);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.title").value(MESSAGE_NOT_READABLE));
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidEmailPassed() throws Exception {
        for (String ie : invalidEmail) {
            userDtoFromAccount_1.setEmail(ie);
            MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                    .contentType(MediaType.APPLICATION_JSON)
                    .accept(MediaType.APPLICATION_JSON)
                    .content(objectMapper.writeValueAsString(userDtoFromAccount_1));
            mockMvc.perform(request)
                    .andDo(print())
                    .andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.errors.email[?(@.message == '%s')]", INVALID_EMAIL).exists());
            verify(service, times(0)).update(any());
        }
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidPasswordPassed() throws Exception {
        for (String ip : invalidPassword) {
            userDtoFromAccount_1.setPassword(ip);
            MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                    .contentType(MediaType.APPLICATION_JSON)
                    .accept(MediaType.APPLICATION_JSON)
                    .content(objectMapper.writeValueAsString(userDtoFromAccount_1));
            mockMvc.perform(request)
                    .andDo(print())
                    .andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.errors.password[?(@.message == '%s')]", INVALID_PASSWORD).exists());
            verify(service, times(0)).update(any());
        }
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getById_shouldFindOneUser_whenUserExistInDatabaseByPassedIdAndHeIsInInnerGroup() throws Exception {
        when(service.findByIdAndAccountId(any(), any())).thenReturn(user_1);
        mockMvc.perform(get(HOST + PORT + API + USER_1_ID))
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.email").value(EMAIL_1))
                .andExpect(jsonPath("$.id").value(USER_1_ID));
        verify(service, times(1)).findByIdAndAccountId(any(), any());
        verify(service, times(0)).findByIdAndAccountIdAndOwnGroupId(any(), any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void getById_shouldFindOneUser_whenUserExistInDatabaseByPassedIdAndHeIsNotInInnerGroup() throws Exception {
        when(service.findByIdAndAccountIdAndOwnGroupId(any(), any(), any())).thenReturn(user_1);
        mockMvc.perform(get(HOST + PORT + API + USER_1_ID))
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.email").value(EMAIL_1))
                .andExpect(jsonPath("$.id").value(USER_1_ID));
        verify(service, times(0)).findByIdAndAccountId(any(), any());
        verify(service, times(1)).findByIdAndAccountIdAndOwnGroupId(any(), any(), any());
    }

    @Test
    @WithAnonymousUser
    void getById_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        when(service.findByIdAndAccountId(any(), any())).thenReturn(user_1);
        mockMvc.perform(get(HOST + PORT + API + USER_1_ID))
                .andDo(print())
                .andExpect(status().isForbidden());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getById_shouldRespondNotFound_whenPassedIdNotExist() throws Exception {
        when(service.findByIdAndAccountId(any(), any())).thenThrow(EntityNotExistException.class);
        mockMvc.perform(get(HOST + PORT + API + USER_1_ID))
                .andDo(print())
                .andExpect(status().isNotFound());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getById_shouldGetStatusBadRequest_whenIdIsInvalid() throws Exception {
        mockMvc.perform(get(HOST + PORT + API + "Abracadabra"))
                .andDo(print())
                .andExpect(status().isBadRequest());
        verify(service, times(0)).findById(any());
    }

    @Test
    @WithAnonymousUser
    void getByFilter_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        Pageable pageable =
                PageRequest.of(Integer.parseInt(PAGE_DEFAULT_VALUE), Integer.parseInt(SIZE_DEFAULT_VALUE),
                        Sort.by("firstName").ascending());
        Page<User> userPageExpected = new PageImpl<>(List.of(user_1, user_2), pageable, 2);
        when(service.findAllByFilter(any(), any())).thenReturn(userPageExpected);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).findAllByFilter(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getByFilter_shouldFindTwoUsers_whenUsersExistInDatabaseByPassedFilter() throws Exception {
        Pageable pageable =
                PageRequest.of(Integer.parseInt(PAGE_DEFAULT_VALUE), Integer.parseInt(SIZE_DEFAULT_VALUE),
                        Sort.by("firstName").ascending());
        Page<User> userPageExpected = new PageImpl<>(List.of(user_1, user_2), pageable, 2);
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
        verify(service, times(1)).findAllByFilter(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getByFilter_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        userFilterDto.setEmail(INVALID_EMAIL_1);
        userFilterDto.setFirstName(INVALID_FIRST_NAME);
        userFilterDto.setSecondName(INVALID_SECOND_NAME);
        userFilterDto.setPhone(INVALID_PHONE);
        String INVALID_SORT_BY = "ERROR";
        userFilterDto.setSortBy(INVALID_SORT_BY);
        String INVALID_DELETED = "ERROR";
        userFilterDto.setDeleted(INVALID_DELETED);
        String INVALID_DIRECTION = "ERROR";
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
        verify(service, times(0)).findAllByFilter(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getByFilter_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidSizeAndPagePassed()
            throws Exception {
        MockHttpServletRequestBuilder request = get(HOST + PORT + API + "?page=-1&size=0")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.title").value(REQUEST_NOT_READABLE));
        verify(service, times(0)).findAllByFilter(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getByFilter_shouldGetStatusBadRequestWithErrorsDescriptions_whenFilterIsEmpty() throws Exception {
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
        verify(service, times(0)).findAllByFilter(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getByFilter_shouldFindTwoUsers_whenFilterIsNew() throws Exception {
        UserFilterDto userFilterDto = new UserFilterDto();
        Pageable pageable =
                PageRequest.of(Integer.parseInt(PAGE_DEFAULT_VALUE), Integer.parseInt(SIZE_DEFAULT_VALUE),
                        Sort.by("firstName").ascending());
        Page<User> userPageExpected = new PageImpl<>(List.of(user_1, user_2), pageable, 2);
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
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getByFilter_shouldFindTwoUsers_whenDefaultFieldsInFilterIsNull() throws Exception {
        userFilterDto.setSortBy(null);
        userFilterDto.setDeleted(null);
        userFilterDto.setDirection(null);
        Pageable pageable =
                PageRequest.of(Integer.parseInt(PAGE_DEFAULT_VALUE), Integer.parseInt(SIZE_DEFAULT_VALUE),
                        Sort.by("firstName").ascending());
        Page<User> userPageExpected = new PageImpl<>(List.of(user_1, user_2), pageable, 2);
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
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void physicalDelete_shouldThrowUnsupportedOperationException_untilMethodWouldBeImplemented() throws Exception {
        mockMvc.perform(delete(HOST + PORT + API + USER_1_ID))
                .andDo(print())
                .andExpect(status().isMethodNotAllowed());
    }

    @Test
    @WithAnonymousUser
    void physicalDelete_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        mockMvc.perform(delete(HOST + PORT + API + USER_1_ID))
                .andDo(print())
                .andExpect(status().isForbidden());
    }

    @Test
    @WithAnonymousUser
    void createCheckAccess_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        mockMvc.perform(post(HOST + PORT + API + CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isForbidden());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void createCheckAccess_shouldGetStatusOk_whenUserWithRoleAdmin() throws Exception {
        mockMvc.perform(post(HOST + PORT + API + CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isOk());
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_2_IS_INNER_GROUP")
    void createCheckAccess_shouldGetStatusOk_whenUserWithRoleAccountOwner() throws Exception {
        mockMvc.perform(post(HOST + PORT + API + CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isOk());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void createCheckAccess_shouldGetStatusForbidden_whenUserWithRoleAuthor() throws Exception {
        mockMvc.perform(post(HOST + PORT + API + CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isForbidden());
    }

    @Test
    @WithAnonymousUser
    void updateCheckAccess_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        mockMvc.perform(put(HOST + PORT + API + CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isForbidden());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void updateCheckAccess_shouldGetStatusOk_whenUserWithRoleAdmin() throws Exception {
        mockMvc.perform(put(HOST + PORT + API + CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isOk());
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_2_IS_INNER_GROUP")
    void updateCheckAccess_shouldGetStatusOk_whenUserWithRoleAccountOwner() throws Exception {
        mockMvc.perform(put(HOST + PORT + API + CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isOk());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void updateCheckAccess_shouldGetStatusForbidden_whenUserWithRoleAuthor() throws Exception {
        mockMvc.perform(put(HOST + PORT + API + CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isForbidden());
    }

}