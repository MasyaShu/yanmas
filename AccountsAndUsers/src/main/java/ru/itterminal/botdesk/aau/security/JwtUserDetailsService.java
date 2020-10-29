package ru.itterminal.botdesk.aau.security;

import java.util.List;

import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Service;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.jwt.JwtUser;

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

        JwtUser jwtUser = new JwtUser()
                .builder()
                .accountId(user.getAccount().getId())
                .groupId(user.getOwnGroup().getId())
                .isInnerGroup(user.getOwnGroup().getIsInner())
                .weightRole(user.getRole().getWeight())
                .username(user.getEmail())
                .password(user.getPassword())
                .authorities(List.of(new SimpleGrantedAuthority(user.getRole().getName())))
                .enabled(user.getEmailVerificationStatus() && !user.getIsArchived() && !user.getAccount().getDeleted())
                .build();

        log.info("in loadUserByUsername - user with username: {} successfully loaded", email);
        return jwtUser;
    }
}
