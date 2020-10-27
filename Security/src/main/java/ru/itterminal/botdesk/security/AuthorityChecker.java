package ru.itterminal.botdesk.security;

import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Component;

import ru.itterminal.botdesk.jwt.JwtUser;

@Component("authorityChecker")
public class AuthorityChecker {

    public boolean is_inner_group(Authentication authentication) {
        JwtUser jwtUser = (JwtUser) authentication.getPrincipal();
        return jwtUser.is_inner_group();
    }

    public boolean is_not_inner_group(Authentication authentication) {
        JwtUser jwtUser = (JwtUser) authentication.getPrincipal();
        return !jwtUser.is_inner_group();
    }


}