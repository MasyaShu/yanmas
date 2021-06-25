package ru.itterminal.yanmas.aau.service.validator.property.check_access_before_update;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Property;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;

@SuppressWarnings("unused")
@Component
public class AccessDeniedIfCurrentUserFromOuterGroupBeforeUpdatePropertyValidator implements EntityValidator<Property> {
    @Override
    public void checkAccessBeforeUpdate(User currentUser) {
        if (currentUser != null) {
            throwAccessDeniedExceptionIfCurrentUserFromOuterGroup(currentUser);
        }
    }
}
