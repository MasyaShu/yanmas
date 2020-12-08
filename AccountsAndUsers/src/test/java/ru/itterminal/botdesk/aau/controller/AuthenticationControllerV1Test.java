package ru.itterminal.botdesk.aau.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static ru.itterminal.botdesk.security.config.TestSecurityConfig.EMAIL_1;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.test.context.support.WithAnonymousUser;
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

import ru.itterminal.botdesk.aau.model.dto.AuthenticationRequestDto;
import ru.itterminal.botdesk.aau.model.dto.ResetPasswordDto;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.aau.util.AAUConstants;
import ru.itterminal.botdesk.commons.exception.RestExceptionHandler;
import ru.itterminal.botdesk.commons.util.CommonConstants;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.security.jwt.JwtProvider;

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

    @SuppressWarnings("unused")
    @MockBean
    private JwtProvider jwtProvider;

    @Mock
    Authentication authentication;

    @Autowired
    private AuthenticationControllerV1 controller;

    @Autowired
    FilterChainProxy springSecurityFilterChain;

    @Autowired
    UserDetailsService userDetailsService;

    private MockMvc mockMvc;

    @BeforeEach
    void setUpBeforeAll() {
        mockMvc = MockMvcBuilders.standaloneSetup(controller)
                .setControllerAdvice(new RestExceptionHandler())
                .apply(SecurityMockMvcConfigurers.springSecurity(springSecurityFilterChain))
                .build();
    }

    private final ObjectMapper objectMapper = new ObjectMapper();
    private static final String HOST = "http://localhost";
    private static final String PORT = ":8081";
    private static final String API = "api/v1/auth/";
    private static final String PASSWORD_RESET = "password-reset";
    private static final String mockEmailVerificationToken = "eyJhbGciOiJIUzI1NiJ9";
    private static final String mockResetPasswordToken = "54434654r5423wfnsdfnw34";
    private static final String VALID_PASSWORD = "pAssword12345";
    private static final String INVALID_PASSWORD = "12445";

    private AuthenticationRequestDto requestDto;
    private ResetPasswordDto resetPasswordDto;

    @BeforeEach
    void setUpBeforeEach() {
        requestDto = AuthenticationRequestDto
                .builder()
                .email(EMAIL_1)
                .password(VALID_PASSWORD)
                .build();
        resetPasswordDto = ResetPasswordDto
                .builder()
                .token(mockResetPasswordToken)
                .password(VALID_PASSWORD)
                .build();
    }

    @Test
    @WithAnonymousUser
    void signIn_shouldGetStatusOk_whenEmailAndPasswordValid() throws Exception {
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
    void signIn_shouldGetJwtAuthenticationException_whenEmailAndPasswordInvalid() throws Exception {
        when(authenticationManager.authenticate(any())).thenThrow(BadCredentialsException.class);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API + "signin")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(requestDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden())
                .andExpect(jsonPath("$.detail").value(AuthenticationControllerV1.INVALID_USERNAME_OR_PASSWORD));
        verify(authenticationManager, times(1)).authenticate(any());
    }

    @Test
    @WithAnonymousUser
    void verifyEmailToken_shouldVerifyEmailToken_whenPassedValidToken() throws Exception {
        doNothing().when(userService).verifyEmailToken(mockEmailVerificationToken);
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + "email-verify?token=" + mockEmailVerificationToken);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").value(AuthenticationControllerV1.EMAIL_IS_VERIFIED));
        verify(userService, times(1)).verifyEmailToken(mockEmailVerificationToken);
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void verifyEmailToken_shouldGetStatusForbidden_whenUserIsNotAnonymous() throws Exception {
        doNothing().when(userService).verifyEmailToken(mockEmailVerificationToken);
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + "email-verify?token=" + mockEmailVerificationToken);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(userService, times(0)).verifyEmailToken(anyString());
    }

    @Test
    @WithAnonymousUser
    void requestPasswordReset_shouldGetStatusOk_whenPassedValidEmail() throws Exception {
        doNothing().when(userService).requestPasswordReset(EMAIL_1);
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + "request-password-reset?email=" + EMAIL_1);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").value(AuthenticationControllerV1.TOKEN_FOR_RESET_PASSWORD_WAS_SENT_TO_EMAIL));
        verify(userService, times(1)).requestPasswordReset(EMAIL_1);
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void requestPasswordReset_shouldGetStatusForbidden_whenUserIsNotAnonymous() throws Exception {
        doNothing().when(userService).requestPasswordReset(EMAIL_1);
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + "request-password-reset?email=" + EMAIL_1);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(userService, times(0)).requestPasswordReset(EMAIL_1);
    }

    @Test
    @WithAnonymousUser
    void passwordReset_shouldGetStatusOk_whenPassedValidTokenAndNewPassword() throws Exception {
        doNothing().when(userService).resetPassword(mockResetPasswordToken, VALID_PASSWORD);
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + PASSWORD_RESET)
                        .contentType(MediaType.APPLICATION_JSON)
                        .accept(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(resetPasswordDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").value(AuthenticationControllerV1.PASSWORD_WAS_RESET_SUCCESSFULLY));
        verify(userService, times(1)).resetPassword(mockResetPasswordToken, VALID_PASSWORD);
    }

    @Test
    @WithAnonymousUser
    void passwordReset_shouldGetStatusBadRequestWithErrorsDescriptions_whenPassedInvalidPassword() throws Exception {
        resetPasswordDto.setPassword(INVALID_PASSWORD);
        doNothing().when(userService).resetPassword(mockResetPasswordToken, INVALID_PASSWORD);
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + PASSWORD_RESET)
                        .contentType(MediaType.APPLICATION_JSON)
                        .accept(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(resetPasswordDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.password[?(@.message == '%s')]", AAUConstants.INVALID_PASSWORD).exists());
        verify(userService, times(0)).resetPassword(mockResetPasswordToken, INVALID_PASSWORD);
    }

    @Test
    @WithAnonymousUser
    void passwordReset_shouldGetStatusBadRequestWithErrorsDescriptions_whenPassedDataIsNull() throws Exception {
        resetPasswordDto.setPassword(null);
        resetPasswordDto.setToken(null);
        doNothing().when(userService).resetPassword(anyString(), anyString());
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + PASSWORD_RESET)
                        .contentType(MediaType.APPLICATION_JSON)
                        .accept(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(resetPasswordDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.token[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.password[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                        .exists());
        verify(userService, times(0)).resetPassword(anyString(), anyString());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void passwordReset_shouldGetStatusForbidden_whenUserIsNotAnonymous() throws Exception {
        doNothing().when(userService).resetPassword(mockResetPasswordToken, INVALID_PASSWORD);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API + PASSWORD_RESET)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(resetPasswordDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(userService, times(0)).resetPassword(mockEmailVerificationToken, EMAIL_1);
    }
}