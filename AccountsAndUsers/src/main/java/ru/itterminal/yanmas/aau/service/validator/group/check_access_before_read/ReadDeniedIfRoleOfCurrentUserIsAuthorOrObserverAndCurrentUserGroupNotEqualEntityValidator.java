package ru.itterminal.yanmas.aau.service.validator.group.check_access_before_read;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;

@Component
public class ReadDeniedIfRoleOfCurrentUserIsAuthorOrObserverAndCurrentUserGroupNotEqualEntityValidator implements EntityValidator<Group> {
    @Override
    public void checkAccessBeforeRead(Group entity, User currentUser) {
        if (currentUser != null
                && currentUser.getRole().getWeight() < Roles.EXECUTOR.getWeight()
                && !currentUser.getGroup().getId().equals(entity.getId())) {
            throw new AccessDeniedException(ACCESS_IS_DENIED_FOR_SEARCHING_BY_PASSED_GROUP_ID);
        }
    }
}
