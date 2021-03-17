package ru.itterminal.botdesk.security.jwt;

import static ru.itterminal.botdesk.security.config.SecurityConfig.isUrlPermittedForAnonymousUser;
import static ru.itterminal.botdesk.security.jwt.JwtFilter.APPLICATION_JSON;
import static ru.itterminal.botdesk.security.jwt.JwtFilter.UNAUTHORISED;

import java.io.IOException;
import java.util.Date;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.http.HttpStatus;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.databind.ObjectMapper;

import ru.itterminal.botdesk.commons.exception.error.ApiError;

@Component
public class CustomAuthenticationEntryPoint implements AuthenticationEntryPoint {
    @Override
    public void commence(HttpServletRequest request, HttpServletResponse response,
                         AuthenticationException authException) throws IOException {
        response.setContentType(APPLICATION_JSON);
        response.setStatus(HttpStatus.UNAUTHORIZED.value());
        var apiError = ApiError.builder()
                .status(HttpStatus.UNAUTHORIZED.value())
                .title(UNAUTHORISED)
                .detail(authException.getMessage())
                .type(authException.getClass().getCanonicalName())
                .path("(" + request.getMethod() + ") " + request.getRequestURI())
                .timestamp(new Date().getTime())
                .build();
        var out = response.getOutputStream();
        var mapper = new ObjectMapper();
        mapper.writeValue(out, apiError);
        out.flush();
    }
}
