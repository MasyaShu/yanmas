package ru.itterminal.botdesk.aau.controller;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.HashSet;
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
import org.springframework.http.MediaType;
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
import ru.itterminal.botdesk.aau.model.dto.RoleDto;
import ru.itterminal.botdesk.aau.model.dto.UserDto;
import ru.itterminal.botdesk.aau.service.impl.RoleServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(UserController.class)
@WebMvcTest
class UserControllerTest {

    @MockBean
    private UserServiceImpl service;

    @Autowired
    private UserController controller;

    private MockMvc mockMvc;

    @BeforeAll
    void setUpBeforeAll() {
        MockitoAnnotations.initMocks(this);
        mockMvc = MockMvcBuilders.standaloneSetup(controller)
                .setControllerAdvice(WebTestConfig.controllerAdvice())
                .build();
    }

    private ObjectMapper objectMapper = new ObjectMapper();
    private static String HOST = "http://localhost";
    private static String PORT = ":8081";
    private static String API = "/v1/user/";
    private static String USER_1_ID = "b3805032-02db-4422-9c0e-4ddba1701811";
    private static String ACCOUNT_1_ID = "d45e0a6e-cb5b-11ea-87d0-0242ac138003";
    private static String GROUP_1_ID = "8f85579e-670c-4c78-88cb-d284bbd473b8";
    private static String EMAIL_1 = "yaneg.ru@gmial.com";
    private static String PASSWORD_1 = "UserUser123";

    private User user_1;
    private Account account_1;
    private Group group_1;
    private Role roleAdmin = new Role(Roles.ADMIN.toString());
    private Role roleSuperAdmin = new Role(Roles.SUPER_ADMIN.toString());
    private Set<Role> roles_1 = Set.of(roleAdmin, roleSuperAdmin);
    private UserDto userDto;

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
        userDto = new UserDto().builder()
                .email(EMAIL_1)
                .password(PASSWORD_1)
                .account(account_1)
                .group(group_1)
                .isArchived(false)
                .roles(roles_1)
                .build();
        userDto.setDeleted(false);
    }

    @Test
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
}