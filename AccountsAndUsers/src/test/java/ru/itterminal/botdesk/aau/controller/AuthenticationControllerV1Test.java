package ru.itterminal.botdesk.aau.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static ru.itterminal.botdesk.aau.controller.AuthenticationControllerV1.EMAIL_IS_VERIFIED;
import static ru.itterminal.botdesk.aau.controller.AuthenticationControllerV1.INVALID_USERNAME_OR_PASSWORD;
import static ru.itterminal.botdesk.aau.controller.AuthenticationControllerV1.PASSWORD_WAS_RESET_SUCCESSFULLY;
import static ru.itterminal.botdesk.aau.controller.AuthenticationControllerV1.TOKEN_FOR_RESET_PASSWORD_WAS_SENT_TO_EMAIL;
import static ru.itterminal.botdesk.config.TestSecurityConfig.ACCOUNT_1_ID;
import static ru.itterminal.botdesk.config.TestSecurityConfig.EMAIL_1;
import static ru.itterminal.botdesk.config.TestSecurityConfig.PASSWORD;

import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
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

import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.dto.AuthenticationRequestDto;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.config.WebTestConfig;
import ru.itterminal.botdesk.config.TestSecurityConfig;
import ru.itterminal.botdesk.jwt.JwtProvider;
import ru.itterminal.botdesk.jwt.JwtUser;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {AuthenticationControllerV1.class, FilterChainProxy.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles("Test")
class AuthenticationControllerV1Test {

    @MockBean
    private UserServiceImpl userService;

    @MockBean
    private AuthenticationManager authenticationManager;

    @MockBean
    private JwtProvider jwtProvider;

    @MockBean
    private BCryptPasswordEncoder encoder;

    @Mock
    Authentication authentication;

    @Autowired
    private AuthenticationControllerV1 controller;

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
    private static String API = "api/v1/auth/";
    private static final String emailVerificationToken = "eyJhbGciOiJIUzI1NiJ9"
            + ".eyJzdWIiOiJhNGI3NWViYi1jNGIyLTQ1YzAtYWY4My1lZDMyNGQ4ZDM1OWQiLCJpYXQiOjE2MDQyOTc2MjEsImV4cCI6MTYwNDMwNjI2MX0.OdzYy23gowGwr1FRcgmtElveFDtjtJdJ6n3pjqmGH0c";
    private AuthenticationRequestDto requestDto;
    private JwtUser jwtUser;

    @BeforeEach
    void setUpBeforeEach() {
        requestDto = AuthenticationRequestDto
                .builder()
                .email(EMAIL_1)
                .password(PASSWORD)
                .build();
        jwtUser = JwtUser
                .builder()
                .username(EMAIL_1)
                .accountId(UUID.fromString(ACCOUNT_1_ID))
                .weightRole(3)
                .authorities(List.of(new SimpleGrantedAuthority(Roles.ACCOUNT_OWNER.toString())))
                .enabled(true)
                .build();
    }

    @Test
    @WithAnonymousUser
    public void signIn_shouldGetStatusOk_whenEmailAndPasswordValid() throws Exception {
        when(authenticationManager.authenticate(any())).thenReturn(authentication);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API + "signin")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(requestDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.email").value(EMAIL_1))
                .andExpect(jsonPath("$.token").doesNotExist());
        verify(authenticationManager, times(1)).authenticate(any());
    }

    @Test
    @WithAnonymousUser
    public void signIn_shouldGetJwtAuthenticationException_whenEmailAndPasswordInvalid() throws Exception {
        when(authenticationManager.authenticate(any())).thenThrow(BadCredentialsException.class);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API + "signin")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(requestDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden())
                .andExpect(jsonPath("$.detail").value(INVALID_USERNAME_OR_PASSWORD));
        verify(authenticationManager, times(1)).authenticate(any());
    }

    @Test
    @WithAnonymousUser
    public void verifyEmailToken_shouldVerifyEmailToken_whenPassedValidToken() throws Exception {
        doNothing().when(userService).verifyEmailToken(emailVerificationToken);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API + "email-verify?token=" + emailVerificationToken);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").value(EMAIL_IS_VERIFIED));
        verify(userService, times(1)).verifyEmailToken(emailVerificationToken);
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    public void verifyEmailToken_shouldGetStatusForbidden_whenUserIsNotAnonymous() throws Exception {
        doNothing().when(userService).verifyEmailToken(emailVerificationToken);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API + "email-verify?token=" + emailVerificationToken);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(userService, times(0)).verifyEmailToken(anyString());
    }

    @Test
    @WithAnonymousUser
    public void requestPasswordReset_shouldGetStatusOk_whenPassedValidEmail() throws Exception {
        doNothing().when(userService).requestPasswordReset(EMAIL_1);
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + "request-password-reset?email=" + EMAIL_1);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").value(TOKEN_FOR_RESET_PASSWORD_WAS_SENT_TO_EMAIL));
        verify(userService, times(1)).requestPasswordReset(EMAIL_1);
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    public void requestPasswordReset_shouldGetStatusForbidden_whenUserIsNotAnonymous() throws Exception {
        doNothing().when(userService).requestPasswordReset(EMAIL_1);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API + "request-password-reset?email=" + EMAIL_1);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(userService, times(0)).requestPasswordReset(EMAIL_1);
    }

    @Test
    @WithAnonymousUser
    public void passwordReset_shouldGetStatusOk_whenPassedValidTokenAndEmail() throws Exception {
        doNothing().when(userService).resetPassword(emailVerificationToken, EMAIL_1);
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + "password-reset?token=" + emailVerificationToken + "&email=" + EMAIL_1);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").value(PASSWORD_WAS_RESET_SUCCESSFULLY));
        verify(userService, times(1)).resetPassword(emailVerificationToken, EMAIL_1);
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    public void passwordReset_shouldGetStatusForbidden_whenUserIsNotAnonymous() throws Exception {
        doNothing().when(userService).resetPassword(emailVerificationToken, EMAIL_1);
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + "password-reset?token=" + emailVerificationToken + "&email=" + EMAIL_1);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(userService, times(0)).resetPassword(emailVerificationToken, EMAIL_1);
    }
}