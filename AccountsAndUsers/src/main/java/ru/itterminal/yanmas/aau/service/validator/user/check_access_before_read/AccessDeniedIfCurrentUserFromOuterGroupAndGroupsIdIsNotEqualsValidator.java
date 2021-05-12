package ru.itterminal.yanmas.aau.service.validator.user.check_access_before_read;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;

import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;

@SuppressWarnings("unused")
@Component
public class AccessDeniedIfCurrentUserFromOuterGroupAndGroupsIdIsNotEqualsValidator implements EntityValidator<User> {

    public static final String ACCESS_IS_DENIED_FOR_SEARCHING_BY_PASSED_ID =
            "Access is denied for searching by passed Id";

    @SuppressWarnings("DuplicatedCode")
    @Override
    public void checkAccessBeforeRead(User entity, User currentUser) {
        if (currentUser != null) {
            var isGroupOfCurrentUserOuter = !currentUser.getGroup().getIsInner();
            var idGroupFromEntity  = entity.getGroup().getId();
            var idGroupOfCurrentUser  = currentUser.getGroup().getId();
            if (isGroupOfCurrentUserOuter && !idGroupFromEntity.equals(idGroupOfCurrentUser)) {
                throw new AccessDeniedException(ACCESS_IS_DENIED_FOR_SEARCHING_BY_PASSED_ID);
            }
        }
    }
}
