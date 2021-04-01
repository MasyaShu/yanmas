package ru.itterminal.yanmas.aau.security;

import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Service;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.security.jwt.JwtUser;

import javax.transaction.Transactional;
import java.util.List;

@Service("jwtUserDetailsService")
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
        log.info("in loadUserByUsername - user with username: {} successfully loaded", email);
        return convertUserIntoJwtUser(user);
    }

    public JwtUser convertUserIntoJwtUser(User user) {
        return JwtUser.builder()
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
    }
}
