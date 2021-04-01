package ru.itterminal.yanmas.security.jwt;

import java.io.IOException;
import java.util.Date;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.http.HttpStatus;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.web.access.AccessDeniedHandler;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.databind.ObjectMapper;

import ru.itterminal.yanmas.commons.exception.error.ApiError;
import ru.itterminal.yanmas.security.config.SecurityConfig;

@Component
public class CustomAccessDeniedHandler implements AccessDeniedHandler {

    public static final String ACCESS_IS_DENIED = "Access is denied";
    public static final String THIS_URL_ALLOWED_FOR_ANONYMOUS_USER = "This url allowed for anonymous user";

    @Override
    public void handle(HttpServletRequest request, HttpServletResponse response,
                       AccessDeniedException accessDeniedException) throws IOException {
        response.setContentType(JwtFilter.APPLICATION_JSON);
        response.setStatus(HttpStatus.FORBIDDEN.value());
        var apiError = ApiError.builder()
                .status(HttpStatus.FORBIDDEN.value())
                .title(ACCESS_IS_DENIED)
                .detail(SecurityConfig.isUrlPermittedForAnonymousUser(request.getRequestURI())
                        ? THIS_URL_ALLOWED_FOR_ANONYMOUS_USER
                        : accessDeniedException.getMessage())
                .type(accessDeniedException.getClass().getCanonicalName())
                .path("(" + request.getMethod() + ") " + request.getRequestURI())
                .timestamp(new Date().getTime())
                .build();
        var out = response.getOutputStream();
        var mapper = new ObjectMapper();
        mapper.writeValue(out, apiError);
        out.flush();

    }
}
