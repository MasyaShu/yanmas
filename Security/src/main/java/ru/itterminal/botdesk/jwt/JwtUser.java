package ru.itterminal.botdesk.jwt;

import java.util.Collection;
import java.util.Objects;
import java.util.UUID;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Builder
public class JwtUser implements UserDetails {

    private  UUID accountId;
    private  UUID groupId;
    private  int weightRole;
    private  String username;
    private  String password;
    private  boolean isInnerGroup;
    private  boolean enabled;
    private  Collection<? extends GrantedAuthority> authorities;

    @Override
    public boolean isAccountNonExpired() {
        return true;
    }

    @Override
    public boolean isAccountNonLocked() {
        return true;
    }

    @Override
    public boolean isCredentialsNonExpired() {
        return true;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof JwtUser)) {
            return false;
        }
        JwtUser jwtUser = (JwtUser) o;
        return weightRole == jwtUser.weightRole &&
                enabled == jwtUser.enabled &&
                isInnerGroup == jwtUser.isInnerGroup &&
                Objects.equals(accountId, jwtUser.accountId) &&
                Objects.equals(groupId, jwtUser.groupId) &&
                Objects.equals(username, jwtUser.username) &&
                Objects.equals(password, jwtUser.password) &&
                Objects.equals(authorities, jwtUser.authorities);
    }

}
