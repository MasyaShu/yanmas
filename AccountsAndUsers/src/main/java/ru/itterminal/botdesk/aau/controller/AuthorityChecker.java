package ru.itterminal.botdesk.aau.controller;

import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Component;

import ru.itterminal.botdesk.jwt.JwtUser;

// TODO delete this Component
@Component("authorityChecker")
public class AuthorityChecker {

    public boolean is_inner_group(Authentication authentication) {
        JwtUser jwtUser = (JwtUser) authentication.getPrincipal();
        return jwtUser.isInnerGroup();
    }

    public boolean is_not_inner_group(Authentication authentication) {
        JwtUser jwtUser = (JwtUser) authentication.getPrincipal();
        return !jwtUser.isInnerGroup();
    }


}