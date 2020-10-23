package ru.itterminal.botdesk.jwt;

import java.util.Base64;
import java.util.Date;
import java.util.UUID;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
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

    @Autowired
    private ApplicationContext appContext;

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

    public String createToken(String email) {
        String causeException = "";
        if (email == null) {
            causeException = EMAIL_IS_NULL;
        }
        if (email != null && email.isEmpty()) {
            causeException = EMAIL_IS_EMPTY;
        }
        if (!causeException.isEmpty()) {
            throw new JwtException(CANT_CREATE_TOKEN_BECAUSE + causeException);
        }
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

    public UUID getUserId(String token) throws Exception {
        String causeException = "";
        if (token == null) {
            causeException = TOKEN_IS_NULL;
        }
        if (token != null && token.isEmpty()) {
            causeException = TOKEN_IS_EMPTY;
        }
        if (!causeException.isEmpty()) {
            throw new JwtException(CANT_GET_USER_ID_FROM_TOKEN_BECAUSE + causeException);
        }
        UUID userId;
        Claims claims = Jwts.parser().setSigningKey(secretToken).parseClaimsJws(token).getBody();
        userId = UUID.fromString((String) claims.getSubject());
        return userId;
    }

    public String getEmail(String token) throws Exception {
        String causeException = "";
        if (token == null) {
            causeException = TOKEN_IS_NULL;
        }
        if (token != null && token.isEmpty()) {
            causeException = TOKEN_IS_EMPTY;
        }
        if (!causeException.isEmpty()) {
            throw new JwtException(CANT_GET_EMAIL_FROM_TOKEN_BECAUSE + causeException);
        }
        Claims claims = Jwts.parser().setSigningKey(secretToken).parseClaimsJws(token).getBody();
        return claims.getSubject();
    }

    public Authentication getAuthentication(String token) throws Exception {
        UserDetailsService userDetailsService =
                (UserDetailsService) appContext.getBean("jwtUserDetailsService");
        UserDetails userDetails = userDetailsService.loadUserByUsername(getEmail(token));
        return new UsernamePasswordAuthenticationToken(userDetails, userDetails.getPassword(),
                userDetails.getAuthorities());
    }

    public String resolveToken(HttpServletRequest req) {
        String bearerToken = req.getHeader("Authorization");
        if (bearerToken != null && bearerToken.startsWith(prefixToken)) {
            return bearerToken.substring(7, bearerToken.length());
        }
        return null;
    }

    public boolean validateToken(String token) throws Exception {
        Jws<Claims> claims = Jwts.parser().setSigningKey(secretToken).parseClaimsJws(token);
        if (claims.getBody().getExpiration().before(new Date())) {
            return false;
        }
        return true;
    }
}
