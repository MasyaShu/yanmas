package ru.itterminal.botdesk.aau.controller.authorityChecker;

import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Component;
import ru.itterminal.botdesk.jwt.JwtUser;

@Component("groupAuthorityChecker")
public class GroupAuthorityChecker {

    public boolean is_inner_group(Authentication authentication) {
        JwtUser jwtUser = (JwtUser) authentication.getPrincipal();
        return jwtUser.is_inner_group();
    }
}