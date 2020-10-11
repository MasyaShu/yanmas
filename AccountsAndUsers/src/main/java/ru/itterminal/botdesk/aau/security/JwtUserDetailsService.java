package ru.itterminal.botdesk.aau.security;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Service;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.security.jwt.JwtUser;
import ru.itterminal.botdesk.aau.security.jwt.JwtUserFactory;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;

@Service
@Slf4j
public class JwtUserDetailsService implements UserDetailsService {

    private final UserServiceImpl userServiceImpl;

    public JwtUserDetailsService(UserServiceImpl userService) {
        this.userServiceImpl = userService;
    }

    @Override
    public UserDetails loadUserByUsername(String email) throws EntityNotExistException {
        User user = userServiceImpl.findByEmail(email)
                .orElseThrow(() -> new EntityNotExistException("User with username: " + email + " not found"));
        JwtUser jwtUser = JwtUserFactory.create(user);
        log.info("in loadUserByUsername - user with username: {} successfully loaded", email);
        return jwtUser;
    }
}
