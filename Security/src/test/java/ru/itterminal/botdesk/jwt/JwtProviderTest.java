package ru.itterminal.botdesk.jwt;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static ru.itterminal.botdesk.config.TestSecurityConfig.EMAIL_1;
import static ru.itterminal.botdesk.jwt.JwtProvider.CANT_CREATE_TOKEN_BECAUSE;
import static ru.itterminal.botdesk.jwt.JwtProvider.CANT_CREATE_TOKEN_IF_USER_ID_IS_NULL;
import static ru.itterminal.botdesk.jwt.JwtProvider.CANT_GET_EMAIL_FROM_TOKEN_BECAUSE;
import static ru.itterminal.botdesk.jwt.JwtProvider.CANT_GET_USER_ID_FROM_TOKEN_BECAUSE;
import static ru.itterminal.botdesk.jwt.JwtProvider.EMAIL_IS_EMPTY;
import static ru.itterminal.botdesk.jwt.JwtProvider.EMAIL_IS_NULL;
import static ru.itterminal.botdesk.jwt.JwtProvider.TOKEN_IS_NULL;

import java.util.List;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import io.jsonwebtoken.JwtException;
import io.jsonwebtoken.SignatureException;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {JwtProvider.class})
@TestPropertySource(properties = {"jwt.token.secret=ksedtob", "jwt.token.expired=8640000", "jwt.token.prefix=Bearer"})
class JwtProviderTest {

    @Autowired
    private JwtProvider jwtProvider;

    @MockBean(name = "jwtUserDetailsService")
    UserDetailsService userDetailsService;

    @Mock
    HttpServletRequest httpServletRequest;

    private JwtUser jwtUser;
    private static final String role = "ADMIN";
    private static final int weightRole = 2;
    private static final UUID USER_ID = UUID.fromString("666fa629-7f0c-4572-a52a-093e3033f030");
    private static final UUID ACCOUNT_ID = UUID.fromString("bfb96afe-a71f-4d59-9143-209424ac0d83");

    @BeforeEach
    void setUpBeforeEach() {
        jwtUser = JwtUser
                .builder()
                .enabled(true)
                .authorities(List.of(new SimpleGrantedAuthority(role)))
                .username(EMAIL_1)
                .weightRole(weightRole)
                .accountId(ACCOUNT_ID)
                .build();
    }

    @Test
    void createToken_shouldCreateToken_whenPassedEmailRoleWeightRoleAndAccountId() throws Throwable {
        String token = jwtProvider.createToken(EMAIL_1);
        assertFalse(token.isEmpty());
    }

    @Test
    void createToken_shouldGetJwtException_whenSomePassedDataIsNull() {
        Throwable throwable = assertThrows(JwtException.class, () -> jwtProvider.createToken((String) null));
        assertTrue(throwable.getMessage().startsWith(CANT_CREATE_TOKEN_BECAUSE));
        assertTrue(throwable.getMessage().contains(EMAIL_IS_NULL));
    }

    @Test
    void createToken_shouldGetJwtException_whenSomePassedDataIsEmpty() {
        Throwable throwable = assertThrows(JwtException.class, () -> jwtProvider.createToken(""));
        assertTrue(throwable.getMessage().startsWith(CANT_CREATE_TOKEN_BECAUSE));
        assertTrue(throwable.getMessage().contains(EMAIL_IS_EMPTY));
    }

    @Test
    void createToken_shouldCreateToken_whenPassedValidUserId() {
        String token = jwtProvider.createToken(USER_ID);
        assertFalse(token.isEmpty());
    }

    @Test
    void createToken_shouldGetJwtException_whenUserIdIsNull() {
        Throwable throwable = assertThrows(JwtException.class, () -> jwtProvider.createToken((UUID) null));
        assertEquals(CANT_CREATE_TOKEN_IF_USER_ID_IS_NULL, throwable.getMessage());
    }

    @Test
    void getUserId_shouldGetUserId_whenPassedValidToken() throws Throwable {
        String token = jwtProvider.createToken(USER_ID);
        UUID userIdFromToken = jwtProvider.getUserId(token);
        assertEquals(USER_ID, userIdFromToken);
    }

    @Test
    void getUserId_shouldGetJwtException_whenPassedTokenIsNull(){
        Throwable throwable = assertThrows(JwtException.class, () -> jwtProvider.getUserId(null));
        assertTrue(throwable.getMessage().startsWith(CANT_GET_USER_ID_FROM_TOKEN_BECAUSE));
        assertTrue(throwable.getMessage().contains(TOKEN_IS_NULL));
    }

    @Test
    void getEmail_shouldGetEmail_whenPassedValidToken() throws Throwable {
        String token = jwtProvider.createToken(EMAIL_1);
        String email = jwtProvider.getEmail(token);
        assertEquals(EMAIL_1, email);
    }

    @Test
    void getEmail_shouldGetJwtException_whenPassedTokenIsNull() {
        Throwable throwable = assertThrows(JwtException.class, () -> jwtProvider.getEmail(null));
        assertTrue(throwable.getMessage().startsWith(CANT_GET_EMAIL_FROM_TOKEN_BECAUSE));
        assertTrue(throwable.getMessage().contains(TOKEN_IS_NULL));
    }

    @Test
    void getAuthentication_shouldGetAuthentication_whenPassedValidData() throws Throwable {
        String token = jwtProvider.createToken(EMAIL_1);
        when(userDetailsService.loadUserByUsername(any())).thenReturn(jwtUser);
        Authentication authentication = jwtProvider.getAuthentication(token);
        assertEquals(jwtUser.getUsername(), ((JwtUser) authentication.getPrincipal()).getUsername());
    }

    @Test
    void resolveToken_shouldGetResolvedToken_whenPassedValidData() throws Throwable {
        String token = jwtProvider.createToken(EMAIL_1);
        String bearerToken = "Bearer " + token;
        when(httpServletRequest.getHeader("Authorization")).thenReturn(bearerToken);
        String resolvedToken = jwtProvider.resolveToken(httpServletRequest);
        assertEquals(token, resolvedToken);
    }

    @Test
    void resolveToken_shouldGetNull_whenPassedInvalidData() throws Throwable {
        String token = jwtProvider.createToken(EMAIL_1);
        when(httpServletRequest.getHeader("Authorization")).thenReturn(token);
        String resolvedToken = jwtProvider.resolveToken(httpServletRequest);
        assertNull(resolvedToken);
    }

    @Test
    void validateTokens_shouldGetTrue_whenPassedValidToken() throws Throwable {
        String token = jwtProvider.createToken(EMAIL_1);
        boolean result = jwtProvider.validateToken(token);
        assertTrue(result);
    }

    @Test
    void validateTokens_shouldGetSignatureException_whenPassedInvalidToken() throws Throwable {
        String token = jwtProvider.createToken(EMAIL_1);
        assertThrows(SignatureException.class, () -> jwtProvider.validateToken(token + "abracadabra"));
    }
}