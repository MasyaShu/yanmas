package ru.itterminal.botdesk.jwt;

import java.io.IOException;
import java.io.OutputStream;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.filter.GenericFilterBean;

import com.fasterxml.jackson.databind.ObjectMapper;

import ru.itterminal.botdesk.commons.exception.error.ApiError;

public class JwtFilter extends GenericFilterBean {

    private JwtProvider jwtProvider;

    public JwtFilter(JwtProvider jwtProvider) {
        this.jwtProvider = jwtProvider;
    }

    @Override
    public void doFilter(ServletRequest req, ServletResponse res, FilterChain filterChain)
            throws IOException, ServletException {

        String token = jwtProvider.resolveToken((HttpServletRequest) req);
        if (token != null ) {
            boolean validateToken = false;
            try {
                validateToken = jwtProvider.validateToken(token);
            } catch (Exception e) {
                res.setContentType("application/json");
                ApiError response = new ApiError(HttpStatus.UNAUTHORIZED ,"Unauthorised", e);
                OutputStream out = res.getOutputStream();
                ObjectMapper mapper = new ObjectMapper();
                mapper.writeValue(out, response);
                out.flush();
                return;
            }
            if (validateToken) {
                Authentication auth = jwtProvider.getAuthentication(token);
                if (auth != null) {
                    SecurityContextHolder.getContext().setAuthentication(auth);
                }
            }
        }
        filterChain.doFilter(req, res);
    }

}
