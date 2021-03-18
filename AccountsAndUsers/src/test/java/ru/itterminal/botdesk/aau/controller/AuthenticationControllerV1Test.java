package ru.itterminal.botdesk.aau.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static ru.itterminal.botdesk.aau.controller.AuthenticationControllerV1.EMAIL_IS_VERIFIED;
import static ru.itterminal.botdesk.aau.controller.AuthenticationControllerV1.EMAIL_WAS_SUCCESSFULLY_UPDATED;
import static ru.itterminal.botdesk.aau.controller.AuthenticationControllerV1.INVALID_USERNAME_OR_PASSWORD;
import static ru.itterminal.botdesk.aau.controller.AuthenticationControllerV1.PASSWORD_WAS_RESET_SUCCESSFULLY;
import static ru.itterminal.botdesk.aau.controller.AuthenticationControllerV1.TOKEN_FOR_RESET_PASSWORD_WAS_SENT_TO_EMAIL;
import static ru.itterminal.botdesk.aau.controller.AuthenticationControllerV1.TOKEN_FOR_UPDATE_EMAIL_WAS_SENT_TO_NEW_EMAIL;
import static ru.itterminal.botdesk.aau.util.AAUConstants.INVALID_EMAIL;
import static ru.itterminal.botdesk.aau.util.AAUConstants.INVALID_PASSWORD;
import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_NOT_BE_NULL;
import static ru.itterminal.botdesk.security.config.TestSecurityConfig.EMAIL_1;

import io.jsonwebtoken.ExpiredJwtException;
import org.hamcrest.Matchers;
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
import ru.itterminal.botdesk.commons.exception.RestExceptionHandler;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.security.jwt.JwtProvider;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {AuthenticationControllerV1.class, FilterChainProxy.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles("Test")
class AuthenticationControllerV1Test {

    public static final String MUST_NOT_BE_EMPTY = "must not be empty";
    public static final String EMAIL = "email";
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
    private static final String mockResetPasswordToken = "54434654r5423";
    private static final String PASSWORD_VALID = "passwordA12345";
    private static final String PASSWORD_INVALID = "12445";

    private AuthenticationRequestDto requestDto;
    private ResetPasswordDto resetPasswordDto;

    @BeforeEach
    void setUpBeforeEach() {
        requestDto = AuthenticationRequestDto
                .builder()
                .email(EMAIL_1)
                .password(PASSWORD_VALID)
                .build();
        resetPasswordDto = ResetPasswordDto
                .builder()
                .token(mockResetPasswordToken)
                .password(PASSWORD_VALID)
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
                .andExpect(jsonPath("$.detail").value(INVALID_USERNAME_OR_PASSWORD));
        verify(authenticationManager, times(1)).authenticate(any());
    }

    @Test
    @WithAnonymousUser
    void verifyEmailToken_shouldVerifyEmailToken_whenPassedValidToken() throws Exception {
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + "email-verify?token=" + mockEmailVerificationToken);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").value(EMAIL_IS_VERIFIED));
        verify(userService, times(1)).verifyEmailToken(mockEmailVerificationToken);
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void verifyEmailToken_shouldGetStatusForbidden_whenUserIsNotAnonymous() throws Exception {
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
    void requestPasswordReset_shouldGetStatusForbidden_whenUserIsNotAnonymous() throws Exception {
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
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + PASSWORD_RESET)
                        .contentType(MediaType.APPLICATION_JSON)
                        .accept(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(resetPasswordDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").value(PASSWORD_WAS_RESET_SUCCESSFULLY));
        verify(userService, times(1)).resetPassword(mockResetPasswordToken, PASSWORD_VALID);
    }

    @Test
    @WithAnonymousUser
    void passwordReset_shouldGetStatusBadRequestWithErrorsDescriptions_whenPassedInvalidPassword() throws Exception {
        resetPasswordDto.setPassword(PASSWORD_INVALID);
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + PASSWORD_RESET)
                        .contentType(MediaType.APPLICATION_JSON)
                        .accept(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(resetPasswordDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.password[?(@.message == '%s')]", INVALID_PASSWORD).exists());
        verify(userService, times(0)).resetPassword(mockResetPasswordToken, PASSWORD_INVALID);
    }

    @Test
    @WithAnonymousUser
    void passwordReset_shouldGetStatusBadRequestWithErrorsDescriptions_whenPassedDataIsNull() throws Exception {
        resetPasswordDto.setPassword(null);
        resetPasswordDto.setToken(null);
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + PASSWORD_RESET)
                        .contentType(MediaType.APPLICATION_JSON)
                        .accept(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(resetPasswordDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.token[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.password[?(@.message == '%s')]", MUST_NOT_BE_NULL)
                        .exists());
        verify(userService, times(0)).resetPassword(anyString(), anyString());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void passwordReset_shouldGetStatusForbidden_whenUserIsNotAnonymous() throws Exception {
        MockHttpServletRequestBuilder request = get(HOST + PORT + API + PASSWORD_RESET)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(resetPasswordDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(userService, times(0)).resetPassword(mockEmailVerificationToken, EMAIL_1);
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_1_IS_INNER_GROUP")
    void requestEmailUpdate_shouldGetStatusOk_whenPassedValidEmail() throws Exception {
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + "request-email-update?newEmail=" + EMAIL_1);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").value(TOKEN_FOR_UPDATE_EMAIL_WAS_SENT_TO_NEW_EMAIL));
        verify(userService, times(1)).requestUpdateEmailOfAccountOwner(EMAIL_1);
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void requestEmailUpdate_shouldGetStatusForbidden_whenUserIsNotAccountOwner() throws Exception {
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + "request-email-update?newEmail=" + EMAIL_1);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(userService, times(0)).requestUpdateEmailOfAccountOwner(EMAIL_1);
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_1_IS_INNER_GROUP")
    void requestEmailUpdate_shouldGetStatusBadRequestWithErrorsDescriptions_whenPassedInvalidNewEmail()
            throws Exception {
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + "request-email-update?newEmail=" + INVALID_EMAIL);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.detail", INVALID_EMAIL).exists());
        verify(userService, times(0)).requestUpdateEmailOfAccountOwner(anyString());
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_1_IS_INNER_GROUP")
    void requestEmailUpdate_shouldGetStatusBadRequestWithErrorsDescriptions_whenPassedEmptyNewEmail() throws Exception {
        var request =
                get(HOST + PORT + API + "request-email-update?newEmail=");
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.detail")
                        .value(Matchers.containsString(
                                MUST_NOT_BE_EMPTY)));
        verify(userService, times(0)).requestUpdateEmailOfAccountOwner(anyString());
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_1_IS_INNER_GROUP")
    void emailUpdate_shouldGetStatusOk_whenPassedValidToken() throws Exception {
        var request =
                get(HOST + PORT + API + "email-update?token=" + mockEmailVerificationToken);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").value(EMAIL_WAS_SUCCESSFULLY_UPDATED));
        verify(userService, times(1)).updateEmailOfAccountOwner(mockEmailVerificationToken);
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void emailUpdate_shouldGetStatusForbidden_whenUserIsNotAccountOwner() throws Exception {
        var request
                = get(HOST + PORT + API + "email-update?token=" + mockEmailVerificationToken);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(userService, times(0)).updateEmailOfAccountOwner(mockEmailVerificationToken);
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_1_IS_INNER_GROUP")
    void emailUpdate_shouldGetStatusBadRequestWithErrorsDescriptions_whenPassedEmptyToken() throws Exception {
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + "email-update?token=");
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.detail")
                        .value(Matchers.containsString(
                                MUST_NOT_BE_EMPTY)));
        verify(userService, times(0)).updateEmailOfAccountOwner(anyString());
    }

    @Test
    @WithAnonymousUser
    void tokenRefresh_shouldIsForbidden_whenTokenHasExpired() throws Exception {
        when(jwtProvider.getTimeAfterTokenExpiration(any())).thenThrow(ExpiredJwtException.class);
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + "token-refresh?token=" + mockEmailVerificationToken);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest());
        verify(jwtProvider, times(1)).getTimeAfterTokenExpiration(any());
    }

    @Test
    @WithAnonymousUser
    void tokenRefresh_shouldNewToken_whenTokenHasNotExpired() throws Exception {
        when(jwtProvider.getTimeAfterTokenExpiration(any())).thenReturn(EMAIL);
        when(jwtProvider.getTimeAfterTokenExpiration(any())).thenReturn(EMAIL);
        when(jwtProvider.createTokenWithJwtUser(any(), any())).thenReturn(mockEmailVerificationToken);
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + "token-refresh?token=" + mockEmailVerificationToken);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.token").exists());
        verify(jwtProvider, times(1)).createTokenWithJwtUser(any(), any());
        verify(jwtProvider, times(1)).getTimeAfterTokenExpiration(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void tokenRefresh_shouldIsForbidden_whenNotAnonymousUser() throws Exception {
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + "token-refresh?token=" + mockEmailVerificationToken);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(jwtProvider, times(0)).getTimeAfterTokenExpiration(any());
    }

    @Test
    @WithAnonymousUser
    void tokenRefresh_shouldNewToken_whenTokenHasExpired() throws Exception {
        when(jwtProvider.getTimeAfterTokenExpiration(any())).thenThrow(ExpiredJwtException.class);
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + "token-refresh?token=" + mockEmailVerificationToken);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest());
        verify(jwtProvider, times(0)).createTokenWithUserEmail(EMAIL);
        verify(jwtProvider, times(1)).getTimeAfterTokenExpiration(any());
    }
}