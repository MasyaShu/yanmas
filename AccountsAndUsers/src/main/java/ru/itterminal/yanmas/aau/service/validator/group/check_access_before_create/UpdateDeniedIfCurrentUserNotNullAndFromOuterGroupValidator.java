package ru.itterminal.yanmas.aau.service.validator.group.check_access_before_create;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;

@Component
public class UpdateDeniedIfCurrentUserNotNullAndFromOuterGroupValidator implements EntityValidator<Group> {
    @Override
    public void checkAccessBeforeCreate(User currentUser) {
        if (currentUser != null) {
            throwAccessDeniedExceptionIfCurrentUserFromOuterGroup(currentUser);
        }
    }
}
