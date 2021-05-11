package ru.itterminal.yanmas.aau.service.validator.group;

import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;

import static ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl.ANONYMOUS;

@Component
public class CheckAccessBeforeReadAccessDeniedForUsersFromOuterGroupsAndGroupEntityNonEqualsGroupUser implements EntityValidator<Group> {
    @Override
    public void checkAccessBeforeRead(Group entity, User currentUser) {
            if (!SecurityContextHolder.getContext().getAuthentication().getName().contains(ANONYMOUS)
                    && Boolean.FALSE.equals(currentUser.getGroup().getIsInner())
                    && !currentUser.getGroup().getId().equals(entity.getId())) {
                throwAccessDeniedExceptionIfCurrentUserFromOuterGroup(currentUser);
            }
    }
}
