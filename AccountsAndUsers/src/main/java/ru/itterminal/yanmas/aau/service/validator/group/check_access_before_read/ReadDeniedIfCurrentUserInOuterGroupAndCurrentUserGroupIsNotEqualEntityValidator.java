package ru.itterminal.yanmas.aau.service.validator.group.check_access_before_read;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;

@Component
public class ReadDeniedIfCurrentUserInOuterGroupAndCurrentUserGroupIsNotEqualEntityValidator implements EntityValidator<Group> {
    @Override
    public void checkAccessBeforeRead(Group entity, User currentUser) {
            if (currentUser != null
                    && Boolean.FALSE.equals(currentUser.getGroup().getIsInner())
                    && !currentUser.getGroup().getId().equals(entity.getId())) {
                throwAccessDeniedExceptionIfCurrentUserFromOuterGroup(currentUser);
            }
    }
}
