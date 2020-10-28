package ru.itterminal.botdesk.aau.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
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
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers;
import org.springframework.security.web.FilterChainProxy;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.dto.GroupDto;
import ru.itterminal.botdesk.aau.model.dto.UserFilterDto;
import ru.itterminal.botdesk.aau.model.spec.GroupSpec;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.commons.config.WebTestConfig;
import ru.itterminal.botdesk.config.TestSecurityConfig;

import java.util.UUID;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static ru.itterminal.botdesk.aau.model.Roles.ACCOUNT_OWNER;
import static ru.itterminal.botdesk.aau.model.Roles.ADMIN;
import static ru.itterminal.botdesk.config.TestSecurityConfig.ACCOUNT_1_ID;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {GroupControllerV1.class, GroupSpec.class, FilterChainProxy.class, AuthorityChecker.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles("Test")
class GroupControllerV1Test {

    @MockBean
    private GroupServiceImpl service;

    @MockBean
    private AccountServiceImpl accountService;

    @Autowired
    private GroupControllerV1 controller;

    @Autowired
    FilterChainProxy springSecurityFilterChain;

    @Autowired
    UserDetailsService userDetailsService;

    @Autowired
    AuthorityChecker authorityChecker;

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
    private static String API = "api/v1/group/";

    private Group group_1;
    private Group group_2;
    private Account account_1;
   // private Group group_1;
    private Role roleAdmin = new Role(ADMIN.toString(), ADMIN.getWeight());
    private Role roleSuperAdmin = new Role(ACCOUNT_OWNER.toString(), ACCOUNT_OWNER.getWeight());
    private GroupDto groypDtoFromAccount_1;
    private UserFilterDto userFilterDto;
    public static String GROUP_1_ID = "d592facb-e6ee-4801-8310-9c7708eb6e6c";
    public static String USER_2_ID = "86840939-c488-448b-a473-cd9e1097dd32";
    private static String GROUP_NAME_1 = "group_1";
    private static String PASSWORD_2 = "UserUser321";

    @BeforeEach
    void setUpBeforeEach() {
        account_1 = new Account();
        account_1.setId(UUID.fromString(ACCOUNT_1_ID));
        group_1 = new Group().builder()
                .isInner(true)
                .isDeprecated(false)
                .name("group_1")
                .build();
        group_1.setId(UUID.fromString(GROUP_1_ID));
        groypDtoFromAccount_1 = new GroupDto().builder()
                .isInner(true)
                .name("group_1")
                .build();
        groypDtoFromAccount_1.setDeleted(false);
        groypDtoFromAccount_1.setIsDeprecated(false);
//        userFilterDto = new UserFilterDto();
//        userFilterDto.setEmail(EMAIL_1);
//        userFilterDto.setFirstName(FIRST_NAME);
//        userFilterDto.setSecondName(SECOND_NAME);
//        userFilterDto.setPhone(PHONE);
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    public void create_shouldCreate_whenValidDataPassed() throws Exception {
        groypDtoFromAccount_1.setDeleted(null);
        groypDtoFromAccount_1.setIsDeprecated(null);
        when(service.create(any())).thenReturn(group_1);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groypDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.id").value(GROUP_1_ID))
                .andExpect(jsonPath("$.name").value(GROUP_NAME_1));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    public void create_shouldGetStatusForbidden_whenGroupIsNotInner() throws Exception {
        groypDtoFromAccount_1.setDeleted(null);
        groypDtoFromAccount_1.setIsDeprecated(null);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groypDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    public void create_shouldGetStatusForbidden_whenRoleUserAuthor() throws Exception {
        groypDtoFromAccount_1.setDeleted(null);
        groypDtoFromAccount_1.setIsDeprecated(null);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groypDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).create(any());
    }

}