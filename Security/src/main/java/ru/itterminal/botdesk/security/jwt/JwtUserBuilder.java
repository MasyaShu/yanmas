package ru.itterminal.botdesk.security.jwt;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

@Component
public class JwtUserBuilder {

    public static final String USER_FROM_OUTER_GROUP_CANNOT_CREATE_OR_UPDATE_THIS_ENTITY =
            "A user from outer group cannot create or update this entity";

    public JwtUser getJwtUser() {
        return (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
    }

    public void throwAccessDeniedExceptionIfCurrentUserFromOuterGroup() {
        if (!SecurityContextHolder.getContext().getAuthentication().getName().contains("anonymous")) {
            JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
            if (!jwtUser.isInnerGroup()) {
                throw new AccessDeniedException(USER_FROM_OUTER_GROUP_CANNOT_CREATE_OR_UPDATE_THIS_ENTITY);
            }
        }
    }
}
