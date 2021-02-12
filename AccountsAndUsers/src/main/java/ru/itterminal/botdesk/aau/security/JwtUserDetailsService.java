package ru.itterminal.botdesk.aau.security;

import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Service;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.security.jwt.JwtUser;

import javax.transaction.Transactional;
import java.util.List;

@Service
@Slf4j
public class JwtUserDetailsService implements UserDetailsService {

    private final UserServiceImpl userServiceImpl;

    public JwtUserDetailsService(UserServiceImpl userService) {
        this.userServiceImpl = userService;
    }

    @Override
    @Transactional
    public UserDetails loadUserByUsername(String email) {
        User user = userServiceImpl.findByEmail(email);

        JwtUser jwtUser = JwtUser
                .builder()
                .id(user.getId())
                .accountId(user.getAccount().getId())
                .groupId(user.getGroup().getId())
                .isInnerGroup(user.getGroup().getIsInner())
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
