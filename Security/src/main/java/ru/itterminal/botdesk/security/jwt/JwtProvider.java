package ru.itterminal.botdesk.security.jwt;

import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekStringForNullOrEmpty;

import java.util.Base64;
import java.util.Date;
import java.util.UUID;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Component;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jws;
import io.jsonwebtoken.JwtException;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;

@Component
public class JwtProvider {

    @Value("${jwt.token.secret}")
    private String secretToken;

    @Value("${jwt.token.prefix}")
    private String prefixToken;

    @Value("${jwt.token.expired}")
    private long validityInMillisecondsToken;

    private final ApplicationContext appContext;

    public static final String CANT_CREATE_TOKEN_BECAUSE = "Can't create token, because ";
    public static final String CANT_GET_USER_ID_FROM_TOKEN_BECAUSE = "Can't get userId from token, because ";
    public static final String CANT_GET_EMAIL_FROM_TOKEN_BECAUSE = "Can't get email from token, because ";
    public static final String EMAIL_IS_NULL = "email is null";
    public static final String TOKEN_IS_NULL = "token is null";
    public static final String EMAIL_IS_EMPTY = "email is empty";
    public static final String TOKEN_IS_EMPTY = "token is empty";
    public static final String CANT_CREATE_TOKEN_IF_USER_ID_IS_NULL = "Can't create token if userId is null";

    public JwtProvider(
            ApplicationContext appContext) {
        this.appContext = appContext;
    }

    @PostConstruct
    protected void init() {
        secretToken = Base64.getEncoder().encodeToString(secretToken.getBytes());
    }

    public String createToken(String email) {
        chekStringForNullOrEmpty(email, EMAIL_IS_NULL, EMAIL_IS_EMPTY, JwtException.class, CANT_CREATE_TOKEN_BECAUSE);
        Claims claims = Jwts.claims().setSubject(email);
        Date now = new Date();
        Date validity = new Date(now.getTime() + validityInMillisecondsToken);
        return Jwts.builder()
                .setClaims(claims)
                .setIssuedAt(now)
                .setExpiration(validity)
                .signWith(SignatureAlgorithm.HS256, secretToken)
                .compact();
    }

    public String createToken(UUID userId) {
        if (userId == null) {
            throw new JwtException(CANT_CREATE_TOKEN_IF_USER_ID_IS_NULL);
        }
        Date now = new Date();
        Date validity = new Date(now.getTime() + validityInMillisecondsToken);
        return Jwts.builder()
                .setSubject(userId.toString())
                .setIssuedAt(now)
                .setExpiration(validity)
                .signWith(SignatureAlgorithm.HS256, secretToken)
                .compact();
    }

    public UUID getUserId(String token) {
        chekStringForNullOrEmpty(token, TOKEN_IS_NULL, TOKEN_IS_EMPTY, JwtException.class,
                CANT_GET_USER_ID_FROM_TOKEN_BECAUSE);
        UUID userId;
        Claims claims = Jwts.parser().setSigningKey(secretToken).parseClaimsJws(token).getBody();
        userId = UUID.fromString(claims.getSubject());
        return userId;
    }

    public String getEmail(String token) {
        chekStringForNullOrEmpty(token, TOKEN_IS_NULL, TOKEN_IS_EMPTY, JwtException.class,
                CANT_GET_EMAIL_FROM_TOKEN_BECAUSE);
        Claims claims = Jwts.parser().setSigningKey(secretToken).parseClaimsJws(token).getBody();
        return claims.getSubject();
    }

    public Authentication getAuthentication(String token) {
        UserDetailsService userDetailsService =
                (UserDetailsService) appContext.getBean("jwtUserDetailsService");
        UserDetails userDetails = userDetailsService.loadUserByUsername(getEmail(token));
        return new UsernamePasswordAuthenticationToken(userDetails, userDetails.getPassword(),
                userDetails.getAuthorities());
    }

    public String resolveToken(HttpServletRequest req) {
        String bearerToken = req.getHeader("Authorization");
        if (bearerToken != null && bearerToken.startsWith(prefixToken)) {
            return bearerToken.substring(7);
        }
        return null;
    }

    public boolean validateToken(String token) {
        Jws<Claims> claims = Jwts.parser().setSigningKey(secretToken).parseClaimsJws(token);
        return !claims.getBody().getExpiration().before(new Date());
    }

}
