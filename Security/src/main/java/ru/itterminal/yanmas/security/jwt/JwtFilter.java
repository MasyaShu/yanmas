package ru.itterminal.yanmas.security.jwt;

import java.io.IOException;
import java.io.OutputStream;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.filter.GenericFilterBean;

import com.fasterxml.jackson.databind.ObjectMapper;

import io.jsonwebtoken.JwtException;
import ru.itterminal.yanmas.commons.exception.error.ApiError;

public class JwtFilter extends GenericFilterBean {
    public static final String APPLICATION_JSON = "application/json";
    public static final String UNAUTHORISED = "Unauthorised";
    public static final String JWT = "JWT";
    private final JwtProvider jwtProvider;
    private static final String REQUEST_IS_REJECTED_BECAUSE_THE_USER_IS_DISABLED =
            "Request is rejected because the user is disabled";

    public JwtFilter(JwtProvider jwtProvider) {
        this.jwtProvider = jwtProvider;
    }

    @Override
    public void doFilter(ServletRequest req, ServletResponse res, FilterChain filterChain)
            throws IOException, ServletException {
        String token = jwtProvider.resolveToken((HttpServletRequest) req);
        if (token != null) {
            boolean validateToken;
            try {
                validateToken = jwtProvider.validateToken(token);
            }
            catch (Exception e) {
                HttpServletRequest request = (HttpServletRequest) req;
                HttpServletResponse httpServletResponse = (HttpServletResponse) res;
                httpServletResponse.setContentType(APPLICATION_JSON);
                httpServletResponse.setStatus(HttpStatus.UNAUTHORIZED.value());
                var apiError = new ApiError(HttpStatus.UNAUTHORIZED, UNAUTHORISED, e);
                apiError.setPath("(" + request.getMethod() + ") "+request.getRequestURI());
                OutputStream out = httpServletResponse.getOutputStream();
                ObjectMapper mapper = new ObjectMapper();
                mapper.writeValue(out, apiError);
                out.flush();
                return;
            }
            if (validateToken) {
                try {
                    Authentication auth = jwtProvider.getAuthentication(token);
                    if (!((JwtUser) auth.getPrincipal()).isEnabled()) {
                        throw new JwtException(REQUEST_IS_REJECTED_BECAUSE_THE_USER_IS_DISABLED);
                    }
                    SecurityContextHolder.getContext().setAuthentication(auth);
                }
                catch (Throwable e) {
                    HttpServletRequest request = (HttpServletRequest) req;
                    HttpServletResponse httpServletResponse = (HttpServletResponse) res;
                    httpServletResponse.setStatus(HttpStatus.FORBIDDEN.value());
                    res.setContentType("application/json");
                    var apiError = new ApiError(HttpStatus.FORBIDDEN, JWT, new JwtException(e.getMessage()));
                    apiError.setPath("(" + request.getMethod() + ") "+request.getRequestURI());
                    OutputStream out = httpServletResponse.getOutputStream();
                    ObjectMapper mapper = new ObjectMapper();
                    mapper.writeValue(out, apiError);
                    out.flush();
                    return;
                }
            }
        }
        filterChain.doFilter(req, res);
    }

}
