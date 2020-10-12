package ru.itterminal.botdesk.jwt;

import java.util.ArrayList;
import java.util.Base64;
import java.util.Date;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
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

    private UserDetailsService userDetailsService;

    private BCryptPasswordEncoder passwordEncoder;

    public JwtProvider(UserDetailsService userDetailsService,
            BCryptPasswordEncoder passwordEncoder) {
        this.userDetailsService = userDetailsService;
        this.passwordEncoder = passwordEncoder;
    }

    @PostConstruct
    protected void init() {
        secretToken = Base64.getEncoder().encodeToString(secretToken.getBytes());
    }

    public String createToken(String email, Set<String> roles, UUID accountId) {

        Claims claims = Jwts.claims().setSubject(email);
        claims.put("roles", roles);
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

    public Authentication getAuthentication(String token) {
        UserDetails userDetails = getUserDetails(token);
        return new UsernamePasswordAuthenticationToken(userDetails, userDetails.getPassword(),
                userDetails.getAuthorities());
    }

    public UserDetails getUserDetails(String token) {
        Claims claims = Jwts.parser().setSigningKey(secretToken).parseClaimsJws(token).getBody();
        String email = claims.getSubject();
        UUID accountId = UUID.fromString((String) claims.get("accountId"));
        Set<String> roles = ((ArrayList<String>) claims.get("roles")).stream().collect(Collectors.toSet());

        JwtUser jwtUser = new JwtUser()
                .builder()
                .accountId(accountId)
                .username(email)
                .authorities(roles.stream()
                        .map(role -> new SimpleGrantedAuthority(role))
                        .collect(Collectors.toList()))
                .enabled(true)
                .build();

        return jwtUser;

    }

    public String getEmail(String token) {
        return Jwts.parser().setSigningKey(secretToken).parseClaimsJws(token).getBody().getSubject();
    }

    public String resolveToken(HttpServletRequest req) {
        String bearerToken = req.getHeader("Authorization");
        if (bearerToken != null && bearerToken.startsWith(prefixToken)) {
            return bearerToken.substring(7, bearerToken.length());
        }
        return null;
    }

    public boolean validateToken(String token) {
        try {
            Jws<Claims> claims = Jwts.parser().setSigningKey(secretToken).parseClaimsJws(token);
            if (claims.getBody().getExpiration().before(new Date())) {
                return false;
            }
            return true;
        }
        catch (Exception e) {
            throw e;
        }
    }
}
