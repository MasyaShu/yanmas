package ru.itterminal.botdesk.aau.controller;

import static org.hamcrest.Matchers.hasSize;
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
import static ru.itterminal.botdesk.commons.model.filter.StringFilter.TypeComparisonForStringFilter.TEXT_EQUALS;
import static ru.itterminal.botdesk.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;

import java.util.List;
import java.util.Set;
import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
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

import com.fasterxml.jackson.databind.ObjectMapper;

import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.dto.UserDtoRequest;
import ru.itterminal.botdesk.aau.model.dto.UserFilterDto;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.RoleServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.aau.util.AAUConstants;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.exception.RestExceptionHandler;
import ru.itterminal.botdesk.commons.model.filter.StringFilter;
import ru.itterminal.botdesk.commons.model.spec.SpecificationsFactory;
import ru.itterminal.botdesk.commons.util.CommonConstants;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {UserControllerV1.class, FilterChainProxy.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class UserControllerV1Test {

    public static final String ERROR = "ERROR";

    @MockBean
    private UserServiceImpl service;

    @SuppressWarnings("unused")
    @MockBean
    private SpecificationsFactory specFactory;

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
    public static final String ROLE_ADMIN_ID = "d7597321-239f-4e06-84a6-853e71574896";
    private static final String INVALID_NAME =
            "12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890";
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
                                                    INVALID_EMAIL_5, INVALID_EMAIL_6, INVALID_EMAIL_7, INVALID_EMAIL_8
    );

    private static final String INVALID_PASSWORD_1 = "Itt12"; //Valid characters, not valid length (5)
    private static final String INVALID_PASSWORD_2 = "itt12"; //not valid characters, not valid length (5)
    private static final String INVALID_PASSWORD_3 = "itt123"; //valid length (6), not valid characters
    private static final String INVALID_PASSWORD_4 = "Itt12Itt12Itt12Itt12Itt12"; //Valid characters, not valid
    // length (25)
    private static final String INVALID_PASSWORD_5 = "IttItt"; //no numbers
    private static final String INVALID_PASSWORD_6 = "ittitt2"; //no uppercase letter
    private static final String INVALID_PASSWORD_7 = "ITTITT2"; //no lowercase letter
    private static final String INVALID_PASSWORD_8 = "IttItt 2"; //space
    private static final String INVALID_PASSWORD_9 = "123123"; //numbers

    private static final String ROLE_ADMIN_DISPLAY_NAME = "role admin display name";
    private static final String PASSWORD_1 = "UserUser123";
    private static final String ROLE_ADMIN_OUT_ID = "role admin outId";
    public static final String GROUP_DISPLAY_NAME = "group display name";
    public static final String GROUP_OUT_ID = "group outId";

    private final Set<String> invalidPassword =
            Set.of(INVALID_PASSWORD_1, INVALID_PASSWORD_2, INVALID_PASSWORD_3, INVALID_PASSWORD_4,
                   INVALID_PASSWORD_5, INVALID_PASSWORD_6, INVALID_PASSWORD_7, INVALID_PASSWORD_8, INVALID_PASSWORD_9
            );

    private User user_1;
    private User user_2;
    private UserDtoRequest userDtoRequestFromAccount_1;
    private final UserTestHelper userTestHelper = new UserTestHelper();

    @BeforeEach
    void setUpBeforeEach() {
        Role roleAdmin = Role.builder()
                .name(Roles.ADMIN.toString())
                .weight(Roles.ADMIN.getWeight())
                .id(UUID.fromString(ROLE_ADMIN_ID))
                .displayName(ROLE_ADMIN_DISPLAY_NAME)
                .outId(ROLE_ADMIN_OUT_ID)
                .build();
        Group group_1 = Group.builder()
                .isInner(true)
                .id(UUID.fromString(TestSecurityConfig.INNER_GROUP_ID))
                .displayName(GROUP_DISPLAY_NAME)
                .outId(GROUP_OUT_ID)
                .build();
        user_1 = userTestHelper.getPredefinedValidEntityList().get(0);
        user_2 = userTestHelper.getPredefinedValidEntityList().get(1);
        userDtoRequestFromAccount_1 = UserDtoRequest
                .builder()
                .email(TestSecurityConfig.EMAIL_1)
                .password(PASSWORD_1)
                .group(group_1.getId())
                .role(roleAdmin.getId())
                .build();
        userDtoRequestFromAccount_1.setDeleted(false);
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldCreate_whenValidDataPassed() throws Exception {
        userDtoRequestFromAccount_1.setDeleted(null);
        when(service.create(any())).thenReturn(user_1);
        when(accountService.findById(any())).thenReturn(user_1.getAccount());
        when(roleService.findById(any())).thenReturn(user_1.getRole());
        when(groupService.findByIdAndAccountId(any())).thenReturn(user_1.getGroup());
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoRequestFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.id").value(user_1.getId().toString()))
                .andExpect(jsonPath("$.email").value(user_1.getEmail()))
                .andExpect(jsonPath("$.password").doesNotExist())
                .andExpect(jsonPath("$.role.outId").value(user_1.getRole().getOutId()))
                .andExpect(jsonPath("$.role.displayName").value(user_1.getRole().getDisplayName()))
                .andExpect(jsonPath("$.group.outId").value(user_1.getGroup().getOutId()))
                .andExpect(jsonPath("$.group.displayName").value(user_1.getGroup().getDisplayName()));
        verify(service, times(1)).create(any());
        verify(accountService, times(1)).findById(any());
        verify(roleService, times(1)).findById(any());
        verify(groupService, times(1)).findByIdAndAccountId(any());
    }

    @Test
    @WithAnonymousUser
    void create_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoRequestFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isUnauthorized());
        verify(service, times(0)).create(any());
        verify(accountService, times(0)).findById(any());
        verify(roleService, times(0)).findById(any());
        verify(groupService, times(0)).findByIdAndAccountId(any());

    }

    @Test
    @WithMockUser(authorities = {"AUTHOR", "OBSERVER", "EXECUTOR"})
    void create_shouldGetStatusForbidden_whenNotAllowedRole() throws Exception {
        userDtoRequestFromAccount_1.setDeleted(null);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoRequestFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).create(any());
        verify(accountService, times(0)).findById(any());
        verify(roleService, times(0)).findById(any());
        verify(groupService, times(0)).findByIdAndAccountId(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        userDtoRequestFromAccount_1.setEmail(INVALID_EMAIL_1);
        userDtoRequestFromAccount_1.setDeleted(true);
        userDtoRequestFromAccount_1.setPassword("");
        userDtoRequestFromAccount_1.setGroup(null);
        userDtoRequestFromAccount_1.setRole(null);
        userDtoRequestFromAccount_1.setName(INVALID_NAME);
        userDtoRequestFromAccount_1.setPhone(INVALID_PHONE);
        userDtoRequestFromAccount_1.setIsArchived(false);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoRequestFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.name[?(@.message =~ /%s.*/)]",
                                           CommonConstants.SIZE_MUST_BE_BETWEEN
                                   ).exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.phone[?(@.message =~ /%s.*/)]",
                                           CommonConstants.SIZE_MUST_BE_BETWEEN
                                   ).exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.deleted[?(@.message == '%s')]",
                                           CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY
                                   ).exists())
                .andExpect(
                        MockMvcResultMatchers
                                .jsonPath(
                                        "$.errors.isArchived[?(@.message == '%s')]",
                                        CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY
                                ).exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.role[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.email[?(@.message == '%s')]", AAUConstants.INVALID_EMAIL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.group[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.password[?(@.message == '%s')]", AAUConstants.INVALID_PASSWORD)
                                   .exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenAllPassedDataIsNull() throws Exception {
        userDtoRequestFromAccount_1.setEmail(null);
        userDtoRequestFromAccount_1.setDeleted(null);
        userDtoRequestFromAccount_1.setPassword(null);
        userDtoRequestFromAccount_1.setGroup(null);
        userDtoRequestFromAccount_1.setRole(null);
        userDtoRequestFromAccount_1.setName(null);
        userDtoRequestFromAccount_1.setPhone(null);
        userDtoRequestFromAccount_1.setIsArchived(false);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoRequestFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(
                        MockMvcResultMatchers
                                .jsonPath(
                                        "$.errors.isArchived[?(@.message == '%s')]",
                                        CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY
                                ).exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.role[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.email[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.group[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.password[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenVersionNotNull() throws Exception {
        userDtoRequestFromAccount_1.setId(UUID.randomUUID());
        userDtoRequestFromAccount_1.setVersion(15);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoRequestFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.id[?(@.message == '%s')]",
                                           CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY
                                   ).exists())
                .andExpect(
                        MockMvcResultMatchers
                                .jsonPath(
                                        "$.errors.version[?(@.message == '%s')]",
                                        CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY
                                ).exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidEmailPassed() throws Exception {
        for (String ie : invalidEmail) {
            userDtoRequestFromAccount_1.setEmail(ie);
            MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                    .contentType(MediaType.APPLICATION_JSON)
                    .accept(MediaType.APPLICATION_JSON)
                    .content(objectMapper.writeValueAsString(userDtoRequestFromAccount_1));
            mockMvc.perform(request)
                    .andDo(print())
                    .andExpect(status().isBadRequest())
                    .andExpect(MockMvcResultMatchers
                                       .jsonPath("$.errors.email[?(@.message == '%s')]", AAUConstants.INVALID_EMAIL)
                                       .exists());
            verify(service, times(0)).create(any());
        }
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidPasswordPassed() throws Exception {
        for (String ip : invalidPassword) {
            userDtoRequestFromAccount_1.setPassword(ip);
            MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                    .contentType(MediaType.APPLICATION_JSON)
                    .accept(MediaType.APPLICATION_JSON)
                    .content(objectMapper.writeValueAsString(userDtoRequestFromAccount_1));
            mockMvc.perform(request)
                    .andDo(print())
                    .andExpect(status().isBadRequest())
                    .andExpect(MockMvcResultMatchers
                                       .jsonPath(
                                               "$.errors.password[?(@.message == '%s')]", AAUConstants.INVALID_PASSWORD)
                                       .exists());
            verify(service, times(0)).create(any());
        }
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldUpdate_whenValidDataPassed() throws Exception {
        userDtoRequestFromAccount_1.setId(user_1.getId());
        userDtoRequestFromAccount_1.setVersion(user_1.getVersion());
        userDtoRequestFromAccount_1.setIsArchived(user_1.getIsArchived());
        when(service.update(any())).thenReturn(user_1);
        when(accountService.findById(any())).thenReturn(user_1.getAccount());
        when(roleService.findById(any())).thenReturn(user_1.getRole());
        when(groupService.findByIdAndAccountId(any())).thenReturn(user_1.getGroup());
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoRequestFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.id").value(user_1.getId().toString()))
                .andExpect(jsonPath("$.email").value(user_1.getEmail()))
                .andExpect(jsonPath("$.password").doesNotExist())
                .andExpect(jsonPath("$.role.outId").value(user_1.getRole().getOutId()))
                .andExpect(jsonPath("$.role.displayName").value(user_1.getRole().getDisplayName()))
                .andExpect(jsonPath("$.group.outId").value(user_1.getGroup().getOutId()))
                .andExpect(jsonPath("$.group.displayName").value(user_1.getGroup().getDisplayName()));
        verify(service, times(1)).update(any());
        verify(accountService, times(1)).findById(any());
        verify(roleService, times(1)).findById(any());
        verify(groupService, times(1)).findByIdAndAccountId(any());
    }

    @Test
    @WithAnonymousUser
    void update_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoRequestFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isUnauthorized());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithMockUser(authorities = {"AUTHOR", "OBSERVER"})
    void update_shouldGetStatusForbidden_whenNotAllowedRole() throws Exception {
        userDtoRequestFromAccount_1.setId(UUID.fromString(USER_1_ID));
        userDtoRequestFromAccount_1.setVersion(0);
        userDtoRequestFromAccount_1.setIsArchived(false);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoRequestFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        userDtoRequestFromAccount_1.setEmail(INVALID_EMAIL_1);
        userDtoRequestFromAccount_1.setDeleted(null);
        userDtoRequestFromAccount_1.setPassword("");
        userDtoRequestFromAccount_1.setGroup(null);
        userDtoRequestFromAccount_1.setRole(null);
        userDtoRequestFromAccount_1.setName(INVALID_NAME);
        userDtoRequestFromAccount_1.setPhone("1234567890123456789012345678901");
        userDtoRequestFromAccount_1.setIsArchived(null);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoRequestFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.id[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.version[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.name[?(@.message =~ /%s.*/)]",
                                           CommonConstants.SIZE_MUST_BE_BETWEEN
                                   ).exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.phone[?(@.message =~ /%s.*/)]",
                                           CommonConstants.SIZE_MUST_BE_BETWEEN
                                   ).exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.deleted[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.isArchived[?(@.message == '%s')]",
                                           CommonConstants.MUST_NOT_BE_NULL
                                   ).exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.role[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.email[?(@.message == '%s')]", AAUConstants.INVALID_EMAIL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.group[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.password[?(@.message == '%s')]", AAUConstants.INVALID_PASSWORD)
                                   .exists());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenAllPassedDataIsNull() throws Exception {
        userDtoRequestFromAccount_1.setEmail(null);
        userDtoRequestFromAccount_1.setDeleted(null);
        userDtoRequestFromAccount_1.setPassword(null);
        userDtoRequestFromAccount_1.setGroup(null);
        userDtoRequestFromAccount_1.setRole(null);
        userDtoRequestFromAccount_1.setName(null);
        userDtoRequestFromAccount_1.setPhone(null);
        userDtoRequestFromAccount_1.setIsArchived(null);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoRequestFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.id[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.version[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.deleted[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.isArchived[?(@.message == '%s')]",
                                           CommonConstants.MUST_NOT_BE_NULL
                                   ).exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.role[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.email[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.group[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenAllPassedDataIsEmpty() throws Exception {
        userDtoRequestFromAccount_1.setEmail("");
        userDtoRequestFromAccount_1.setPassword("");
        userDtoRequestFromAccount_1.setName("");
        userDtoRequestFromAccount_1.setPhone("");
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoRequestFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.id[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.email[?(@.message == '%s')]", AAUConstants.INVALID_EMAIL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.phone[?(@.message =~ /%s.*/)]",
                                           CommonConstants.SIZE_MUST_BE_BETWEEN
                                   ).exists());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenVersionIsNegative() throws Exception {
        userDtoRequestFromAccount_1.setVersion(-15);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userDtoRequestFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(
                        MockMvcResultMatchers
                                .jsonPath(
                                        "$.errors.version[?(@.message == '%s')]",
                                        CommonConstants.MUST_BE_GREATER_THAN_OR_EQUAL_TO_0
                                )
                                .exists());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidIdPassed() throws Exception {
        userDtoRequestFromAccount_1.setId(UUID.fromString(USER_1_ID));
        userDtoRequestFromAccount_1.setVersion(1);
        String json = objectMapper.writeValueAsString(userDtoRequestFromAccount_1);
        json = json.replace(USER_1_ID, "abracadabra");
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(json);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.title").value(CommonConstants.MESSAGE_NOT_READABLE));
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidEmailPassed() throws Exception {
        for (String ie : invalidEmail) {
            userDtoRequestFromAccount_1.setEmail(ie);
            MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                    .contentType(MediaType.APPLICATION_JSON)
                    .accept(MediaType.APPLICATION_JSON)
                    .content(objectMapper.writeValueAsString(userDtoRequestFromAccount_1));
            mockMvc.perform(request)
                    .andDo(print())
                    .andExpect(status().isBadRequest())
                    .andExpect(MockMvcResultMatchers
                                       .jsonPath("$.errors.email[?(@.message == '%s')]", AAUConstants.INVALID_EMAIL)
                                       .exists());
            verify(service, times(0)).update(any());
        }
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidPasswordPassed() throws Exception {
        for (String ip : invalidPassword) {
            userDtoRequestFromAccount_1.setPassword(ip);
            MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                    .contentType(MediaType.APPLICATION_JSON)
                    .accept(MediaType.APPLICATION_JSON)
                    .content(objectMapper.writeValueAsString(userDtoRequestFromAccount_1));
            mockMvc.perform(request)
                    .andDo(print())
                    .andExpect(status().isBadRequest())
                    .andExpect(MockMvcResultMatchers
                                       .jsonPath(
                                               "$.errors.password[?(@.message == '%s')]", AAUConstants.INVALID_PASSWORD)
                                       .exists());
            verify(service, times(0)).update(any());
        }
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getById_shouldFindOneUser_whenUserExistInDatabaseByPassedIdAndHeIsInInnerGroup() throws Exception {
        when(service.findByIdAndAccountId(any())).thenReturn(user_1);
        mockMvc.perform(get(HOST + PORT + API + USER_1_ID))
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.email").value(user_1.getEmail()))
                .andExpect(jsonPath("$.id").value(user_1.getId().toString()))
                .andExpect(jsonPath("$.role.outId").value(user_1.getRole().getOutId()))
                .andExpect(jsonPath("$.group.outId").value(user_1.getGroup().getOutId()));
        verify(service, times(1)).findByIdAndAccountId(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void getById_shouldFindOneUser_whenUserExistInDatabaseByPassedIdAndHeIsNotInInnerGroup() throws Exception {
        when(service.findByIdAndAccountId(any())).thenReturn(user_1);
        mockMvc.perform(get(HOST + PORT + API + USER_1_ID))
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.email").value(user_1.getEmail()))
                .andExpect(jsonPath("$.id").value(user_1.getId().toString()))
                .andExpect(jsonPath("$.role.outId").value(user_1.getRole().getOutId()))
                .andExpect(jsonPath("$.role.displayName").value(user_1.getRole().getDisplayName()))
                .andExpect(jsonPath("$.group.outId").value(user_1.getGroup().getOutId()))
                .andExpect(jsonPath("$.group.displayName").value(user_1.getGroup().getDisplayName()));
        verify(service, times(1)).findByIdAndAccountId(any());
    }

    @Test
    @WithAnonymousUser
    void getById_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        when(service.findByIdAndAccountId(any())).thenReturn(user_1);
        mockMvc.perform(get(HOST + PORT + API + USER_1_ID))
                .andDo(print())
                .andExpect(status().isUnauthorized());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getById_shouldRespondNotFound_whenPassedIdNotExist() throws Exception {
        when(service.findByIdAndAccountId(any())).thenThrow(EntityNotExistException.class);
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
        Pageable pageable = PageRequest.of(
                Integer.parseInt(BaseController.PAGE_DEFAULT_VALUE),
                Integer.parseInt(BaseController.SIZE_DEFAULT_VALUE),
                Sort.by("name").ascending()
        );
        Page<User> expectedPageOfUsers = new PageImpl<>(List.of(user_1), pageable, 1);
        var userFilterDto = UserFilterDto.builder().build();
        when(service.findAllByFilter(any(), any())).thenReturn(expectedPageOfUsers);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isUnauthorized());
        verify(service, times(0)).findAllByFilter(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getByFilter_shouldFindTwoUsers_whenUsersExistInDatabaseByPassedFilter() throws Exception {
        Pageable pageable =
                PageRequest.of(Integer.parseInt(BaseController.PAGE_DEFAULT_VALUE), Integer.parseInt(
                        BaseController.SIZE_DEFAULT_VALUE),
                               Sort.by("name").ascending()
                );
        Page<User> expectedPageOfUsers = new PageImpl<>(List.of(user_1, user_2), pageable, 2);
        when(service.findAllByFilter(any(), any())).thenReturn(expectedPageOfUsers);
        var userFilterDto = UserFilterDto.builder().build();
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].id").value(user_1.getId().toString()))
                .andExpect(jsonPath("$.content[1].id").value(user_2.getId().toString()))
                .andExpect(jsonPath("$.content", hasSize(2)));
        verify(service, times(1)).findAllByFilter(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getByFilter_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        var emailFilter = StringFilter.builder()
                .value(INVALID_EMAIL_1)
                .typeComparison(TEXT_EQUALS.toString())
                .build();
        var nameFilter = StringFilter.builder()
                .value(INVALID_NAME)
                .typeComparison(TEXT_EQUALS.toString())
                .build();
        var phoneFilter = StringFilter.builder()
                .value(INVALID_PHONE)
                .typeComparison(TEXT_EQUALS.toString())
                .build();
        var userFilterDto = UserFilterDto.builder()
                .email(emailFilter)
                .name(nameFilter)
                .phone(phoneFilter)
                .sortDirection(ERROR)
                .sortByFields(List.of(ERROR))
                .build();
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.name[?(@.message =~ /%s.*/)]",
                                           CommonConstants.SIZE_MUST_BE_BETWEEN
                                   ).exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.email[?(@.message == '%s')]", AAUConstants.INVALID_EMAIL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.sortByFields[?(@.message =~ /.*%s.*/)]",
                                           CommonConstants.DO_NOT_MATCH_THE_AVAILABLE_SORT_VALUES
                                   )
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.sortDirection[?(@.message == '%s')]",
                                           CommonConstants.MUST_BE_ANY_OF_ASC_DESC
                                   ).exists());
        verify(service, times(0)).findAllByFilter(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getByFilter_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidSizeAndPagePassed()
            throws Exception {
        var userFilterDto = UserFilterDto.builder().build();
        MockHttpServletRequestBuilder request = get(HOST + PORT + API + "?page=-1&size=0")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.title").value(CommonConstants.REQUEST_NOT_READABLE));
        verify(service, times(0)).findAllByFilter(any(), any());
    }

    @Test
    @WithAnonymousUser
    void createCheckAccess_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        mockMvc.perform(post(HOST + PORT + API + BaseController.CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isUnauthorized());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void createCheckAccess_shouldGetStatusOk_whenUserWithRoleAdmin() throws Exception {
        mockMvc.perform(post(HOST + PORT + API + BaseController.CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isOk());
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_2_IS_INNER_GROUP")
    void createCheckAccess_shouldGetStatusOk_whenUserWithRoleAccountOwner() throws Exception {
        mockMvc.perform(post(HOST + PORT + API + BaseController.CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isOk());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void createCheckAccess_shouldGetStatusForbidden_whenUserWithRoleAuthor() throws Exception {
        mockMvc.perform(post(HOST + PORT + API + BaseController.CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isForbidden());
    }

    @Test
    @WithAnonymousUser
    void updateCheckAccess_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        mockMvc.perform(put(HOST + PORT + API + BaseController.CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isUnauthorized());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void updateCheckAccess_shouldGetStatusOk_whenUserWithRoleAdmin() throws Exception {
        mockMvc.perform(put(HOST + PORT + API + BaseController.CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isOk());
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_2_IS_INNER_GROUP")
    void updateCheckAccess_shouldGetStatusOk_whenUserWithRoleAccountOwner() throws Exception {
        mockMvc.perform(put(HOST + PORT + API + BaseController.CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isOk());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void updateCheckAccess_shouldGetStatusForbidden_whenUserWithRoleAuthor() throws Exception {
        mockMvc.perform(put(HOST + PORT + API + BaseController.CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isForbidden());
    }

}