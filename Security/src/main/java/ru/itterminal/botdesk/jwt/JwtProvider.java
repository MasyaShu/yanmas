package ru.itterminal.botdesk.jwt;

import java.util.Base64;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jws;
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

    public static final String INVALID_TOKEN = "invalid token";

    @PostConstruct
    protected void init() {
        secretToken = Base64.getEncoder().encodeToString(secretToken.getBytes());
    }

    public String createToken(String email, String role, int weightRole, UUID accountId) {
        Claims claims = Jwts.claims().setSubject(email);
        claims.put("role", role);
        claims.put("weightRole", weightRole);
        claims.put("accountId", accountId);
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
        Date now = new Date();
        Date validity = new Date(now.getTime() + validityInMillisecondsToken);
        return Jwts.builder()
                .setSubject(userId.toString())
                .setIssuedAt(now)
                .setExpiration(validity)
                .signWith(SignatureAlgorithm.HS256, secretToken)
                .compact();
    }

    public Authentication getAuthentication(String token) throws Exception {
        UserDetails userDetails = getUserDetails(token);
        return new UsernamePasswordAuthenticationToken(userDetails, userDetails.getPassword(),
                userDetails.getAuthorities());
    }

    public UUID getUserId(String token) throws Exception {
        UUID userId;
        Claims claims = Jwts.parser().setSigningKey(secretToken).parseClaimsJws(token).getBody();
        userId = UUID.fromString((String) claims.getSubject());
        return userId;
    }

    public UserDetails getUserDetails(String token)  throws Exception {
        Claims claims = Jwts.parser().setSigningKey(secretToken).parseClaimsJws(token).getBody();
        String email = claims.getSubject();
        UUID accountId = UUID.fromString((String) claims.get("accountId"));
        int weightRole = (int) claims.get("weightRole");
        String role = (String) claims.get("role");

        JwtUser jwtUser = new JwtUser()
                .builder()
                .accountId(accountId)
                .weightRole(weightRole)
                .username(email)
                .authorities(List.of(new SimpleGrantedAuthority(role)))
                .enabled(true)
                .build();

        return jwtUser;

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
