package ru.itterminal.botdesk.security.jwt;

import io.jsonwebtoken.*;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletRequest;
import java.util.Base64;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekStringForNullOrEmpty;

@Component
public class JwtProvider {

    @Value("${jwt.token.secret}")
    private String secretToken;

    @Value("${jwt.token.prefix}")
    private String prefixToken;

    @Value("${jwt.token.expired}")
    private long validityInMillisecondsToken;

    public static final String CANT_CREATE_TOKEN_BECAUSE = "Can't create token, because ";
    public static final String CANT_GET_USER_ID_FROM_TOKEN_BECAUSE = "Can't get userId from token, because ";
    public static final String CANT_GET_EMAIL_FROM_TOKEN_BECAUSE = "Can't get email from token, because ";
    public static final String EMAIL_IS_NULL = "email is null";
    public static final String TOKEN_IS_NULL = "token is null";
    public static final String EMAIL_IS_EMPTY = "email is empty";
    public static final String TOKEN_IS_EMPTY = "token is empty";
    public static final String CANT_CREATE_TOKEN_IF_USER_ID_IS_NULL = "Can't create token if userId is null";

    @PostConstruct
    protected void init() {
        secretToken = Base64.getEncoder().encodeToString(secretToken.getBytes());
    }

    public String createTokenWithJwtUser(String email, JwtUser jwtUser) {
        chekStringForNullOrEmpty(email, EMAIL_IS_NULL, EMAIL_IS_EMPTY, JwtException.class, CANT_CREATE_TOKEN_BECAUSE);
        Claims claims = Jwts.claims().setSubject(email);
        claims.put("id", jwtUser.getId().toString());
        claims.put("accountId", jwtUser.getAccountId().toString());
        claims.put("groupId", jwtUser.getGroupId().toString());
        claims.put("isInnerGroup", jwtUser.isInnerGroup());
        claims.put("weightRole", jwtUser.getWeightRole());
        claims.put("username", jwtUser.getUsername());
        claims.put("authorities", jwtUser.getAuthorities().stream().findFirst().get().toString());
        claims.put("enabled", jwtUser.isEnabled());
        Date now = new Date();
        Date validity = new Date(now.getTime() + validityInMillisecondsToken);
        return Jwts.builder()
                .setClaims(claims)
                .setIssuedAt(now)
                .setExpiration(validity)
                .signWith(SignatureAlgorithm.HS256, secretToken)
                .compact();
    }

    public String createTokenWithUserId(UUID userId) {
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

    public String createTokenWithUserEmail(String email) {
        chekStringForNullOrEmpty(email, EMAIL_IS_NULL, EMAIL_IS_EMPTY, JwtException.class, CANT_CREATE_TOKEN_BECAUSE);
        Date now = new Date();
        Date validity = new Date(now.getTime() + validityInMillisecondsToken);
        return Jwts.builder()
                .setSubject(email)
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
        UserDetails userDetails = getUserDetails(token);
        return new UsernamePasswordAuthenticationToken(userDetails, userDetails.getPassword(),
                userDetails.getAuthorities());
    }

    private UserDetails getUserDetails(String token) {
        Claims claims = Jwts.parser().setSigningKey(secretToken).parseClaimsJws(token).getBody();
        return JwtUser.builder()
                .id(UUID.fromString(claims.get("id", String.class)))
                .accountId(UUID.fromString(claims.get("accountId", String.class)))
                .groupId(UUID.fromString(claims.get("groupId", String.class)))
                .isInnerGroup(claims.get("isInnerGroup", Boolean.class))
                .weightRole(claims.get("weightRole",Integer.class))
                .username(claims.get("username",String.class))
                .authorities(List.of(new SimpleGrantedAuthority(claims.get("authorities",String.class))))
                .enabled(claims.get("enabled", Boolean.class))
                .build();

    }

    public String resolveToken(HttpServletRequest req) {
        String bearerToken = req.getHeader("Authorization");
        if (bearerToken != null && bearerToken.startsWith(prefixToken)) {
            return bearerToken.substring(7);
        }
        return null;
    }

    public boolean validateToken(String token) {
        Jwts.parser().setSigningKey(secretToken).parseClaimsJws(token);
        return true;
    }

    public String getTimeAfterTokenExpiration(String token) {
        var validityInSecondsToken = validityInMillisecondsToken/1000;
        Jws<Claims> claims = Jwts.parser().setAllowedClockSkewSeconds(validityInSecondsToken).setSigningKey(secretToken).parseClaimsJws(token);
        return claims.getBody().getSubject();
    }

    @Deprecated
    public String createExpiredTokenWithUserEmail(String email, long milliseconds) {
        chekStringForNullOrEmpty(email, EMAIL_IS_NULL, EMAIL_IS_EMPTY, JwtException.class, CANT_CREATE_TOKEN_BECAUSE);
        Date now = new Date(new Date().getTime()- milliseconds);
        Date validity = new Date(now.getTime() + validityInMillisecondsToken);
        return Jwts.builder()
                .setSubject(email)
                .setIssuedAt(now)
                .setExpiration(validity)
                .signWith(SignatureAlgorithm.HS256, secretToken)
                .compact();
    }
}
