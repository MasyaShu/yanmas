package ru.itterminal.botdesk.aau.controller;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import ru.itterminal.botdesk.aau.model.dto.AuthenticationRequestDto;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.JwtAuthenticationException;
import ru.itterminal.botdesk.jwt.JwtProvider;
import ru.itterminal.botdesk.jwt.JwtUser;

@RestController
@RequestMapping(value = "/api/v1/auth/")
public class AuthenticationControllerV1 {

    private final AuthenticationManager authenticationManager;

    private final JwtProvider jwtProvider;

    private final UserServiceImpl userService;

    @Value("${jwt.token.prefix}")
    private String prefixToken;

    @Autowired
    public AuthenticationControllerV1(AuthenticationManager authenticationManager, JwtProvider jwtProvider,
            UserServiceImpl userService) {
        this.authenticationManager = authenticationManager;
        this.jwtProvider = jwtProvider;
        this.userService = userService;
    }

    // TODO add tests
    @PostMapping("signin")
    public ResponseEntity login(@RequestBody AuthenticationRequestDto requestDto) {
        try {
            String email = requestDto.getEmail();
            Authentication authentication = authenticationManager
                    .authenticate(new UsernamePasswordAuthenticationToken(email, requestDto.getPassword()));
            JwtUser jwtUser =  (JwtUser) authentication.getPrincipal();
            String role = jwtUser.getAuthorities().stream()
                    .map(Object::toString)
                    .findFirst().get();
            UUID accountId = jwtUser.getAccountId();
            String token = jwtProvider.createToken(email, role, accountId);
            Map<Object, Object> response = new HashMap<>();
            response.put("email", email);
            response.put("token", token);
            return ResponseEntity.ok(response);
        }
        catch (AuthenticationException e) {
            throw new JwtAuthenticationException("invalid username or password");
        }
    }
}