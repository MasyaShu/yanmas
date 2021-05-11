package ru.itterminal.yanmas.aau.service.validator.group;

import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;

import static ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl.ANONYMOUS;

@Component
public class CheckAccessBeforeCreateAccessDeniedForUsersFromOuterGroupsAndNotAnonymous implements EntityValidator<Group> {
    @Override
    public void checkAccessBeforeCreate(User currentUser) {
       if (!SecurityContextHolder.getContext().getAuthentication().getName().contains(ANONYMOUS)) {
            throwAccessDeniedExceptionIfCurrentUserFromOuterGroup(currentUser);
        }
    }
}
