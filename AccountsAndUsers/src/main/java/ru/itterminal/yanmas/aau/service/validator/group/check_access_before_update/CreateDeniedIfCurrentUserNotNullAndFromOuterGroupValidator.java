package ru.itterminal.yanmas.aau.service.validator.group.check_access_before_update;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;

@Component
public class CreateDeniedIfCurrentUserNotNullAndFromOuterGroupValidator implements EntityValidator<Group> {
    @Override
    public void checkAccessBeforeUpdate(User currentUser) {
        if (currentUser != null) {
            throwAccessDeniedExceptionIfCurrentUserFromOuterGroup(currentUser);
        }
    }
}
