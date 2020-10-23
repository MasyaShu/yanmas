package ru.itterminal.botdesk.jwt;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.booleanThat;
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
    private JwtProvider jwtProvider = new JwtProvider();

    @MockBean(name = "jwtUserDetailsService")
    UserDetailsService userDetailsService;

    @Mock
    HttpServletRequest httpServletRequest;

    private JwtUser jwtUser;
    private static final String role = "ADMIN";
    private static final int weightRole = 2;
    private static final String password = "12345";
    private static UUID USER_ID = UUID.fromString("666fa629-7f0c-4572-a52a-093e3033f030");
    private static UUID ACCOUNT_ID = UUID.fromString("bfb96afe-a71f-4d59-9143-209424ac0d83");
    private static final String JWT_STRING_ARGUMENT_CANNOT_BE_NULL_OR_EMPTY =
            "JWT String argument cannot be null or empty.";

    @BeforeEach
    void setUpBeforeEach() {
        jwtUser = new JwtUser().builder()
                .enabled(true)
                .authorities(List.of(new SimpleGrantedAuthority(role)))
                .username(EMAIL_1)
                .weightRole(weightRole)
                .accountId(ACCOUNT_ID)
                .build();
    }

    @Test
    public void createToken_shouldCreateToken_whenPassedEmailRoleWeightRoleAndAccountId() {
        String token = jwtProvider.createToken(EMAIL_1);
        assertTrue(!token.isEmpty());
    }

    @Test
    public void createToken_shouldGetJwtException_whenSomePassedDataIsNull() {
        Throwable throwable = assertThrows(JwtException.class, () -> jwtProvider.createToken((String) null));
        assertTrue(throwable.getMessage().startsWith(CANT_CREATE_TOKEN_BECAUSE));
        assertTrue(throwable.getMessage().contains(EMAIL_IS_NULL));
    }

    @Test
    public void createToken_shouldGetJwtException_whenSomePassedDataIsEmpty() {
        Throwable throwable = assertThrows(JwtException.class, () -> jwtProvider.createToken(""));
        assertTrue(throwable.getMessage().startsWith(CANT_CREATE_TOKEN_BECAUSE));
        assertTrue(throwable.getMessage().contains(EMAIL_IS_EMPTY));
    }

    @Test
    public void createToken_shouldCreateToken_whenPassedValidUserId() {
        String token = jwtProvider.createToken(USER_ID);
        assertTrue(!token.isEmpty());
    }

    @Test
    public void createToken_shouldGetJwtException_whenUserIdIsNull() {
        Throwable throwable = assertThrows(JwtException.class, () -> jwtProvider.createToken((UUID) null));
        assertTrue(throwable.getMessage().equals(CANT_CREATE_TOKEN_IF_USER_ID_IS_NULL));
    }

    @Test
    public void getUserId_shouldGetUserId_whenPassedValidToken() throws Exception {
        String token = jwtProvider.createToken(USER_ID);
        UUID userIdFromToken = jwtProvider.getUserId(token);
        assertEquals(USER_ID, userIdFromToken);
    }

    @Test
    public void getUserId_shouldGetJwtException_whenPassedTokenIsNull() throws Exception {
        Throwable throwable = assertThrows(JwtException.class, () -> jwtProvider.getUserId(null));
        assertTrue(throwable.getMessage().startsWith(CANT_GET_USER_ID_FROM_TOKEN_BECAUSE));
        assertTrue(throwable.getMessage().contains(TOKEN_IS_NULL));
    }

    @Test
    public void getEmail_shouldGetEmail_whenPassedValidToken() throws Exception {
        String token = jwtProvider.createToken(EMAIL_1);
        String email = jwtProvider.getEmail(token);
        assertEquals(EMAIL_1, email);
    }

    @Test
    public void getEmail_shouldGetJwtException_whenPassedTokenIsNull() throws Exception {
        Throwable throwable = assertThrows(JwtException.class, () -> jwtProvider.getEmail(null));
        assertTrue(throwable.getMessage().startsWith(CANT_GET_EMAIL_FROM_TOKEN_BECAUSE));
        assertTrue(throwable.getMessage().contains(TOKEN_IS_NULL));
    }

    @Test
    public void getAuthentication_shouldGetAuthentication_whenPassedValidData() throws Exception {
        String token = jwtProvider.createToken(EMAIL_1);
        when(userDetailsService.loadUserByUsername(any())).thenReturn(jwtUser);
        Authentication authentication = jwtProvider.getAuthentication(token);
        assertEquals(jwtUser.getUsername(), ((JwtUser) authentication.getPrincipal()).getUsername());
    }

    @Test
    public void resolveToken_shouldGetResolvedToken_whenPassedValidData() {
        String token = jwtProvider.createToken(EMAIL_1);
        String bearerToken = "Bearer " + token;
        when(httpServletRequest.getHeader("Authorization")).thenReturn(bearerToken);
        String resolvedToken = jwtProvider.resolveToken(httpServletRequest);
        assertEquals(token, resolvedToken);
    }

    @Test
    public void resolveToken_shouldGetNull_whenPassedInvalidData() {
        String token = jwtProvider.createToken(EMAIL_1);
        when(httpServletRequest.getHeader("Authorization")).thenReturn(token);
        String resolvedToken = jwtProvider.resolveToken(httpServletRequest);
        assertNull(resolvedToken);
    }

    @Test
    public void validateTokens_houldGetTrue_whenPassedValidToken() throws Exception {
        String token = jwtProvider.createToken(EMAIL_1);
        boolean result = jwtProvider.validateToken(token);
        assertTrue(result);
    }

    @Test
    public void validateTokens_houldGetSignatureException_whenPassedInvalidToken() throws Exception {
        String token = jwtProvider.createToken(EMAIL_1);
        assertThrows(SignatureException.class, ()-> jwtProvider.validateToken(token + "abracadabra"));
    }
}