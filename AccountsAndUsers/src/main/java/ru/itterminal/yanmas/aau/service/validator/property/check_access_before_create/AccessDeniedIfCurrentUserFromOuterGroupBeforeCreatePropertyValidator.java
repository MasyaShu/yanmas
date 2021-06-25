package ru.itterminal.yanmas.aau.service.validator.property.check_access_before_create;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Property;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;

@SuppressWarnings("unused")
@Component
public class AccessDeniedIfCurrentUserFromOuterGroupBeforeCreatePropertyValidator implements EntityValidator<Property> {
    @Override
    public void checkAccessBeforeCreate(User currentUser) {
        if (currentUser != null) {
            throwAccessDeniedExceptionIfCurrentUserFromOuterGroup(currentUser);
        }
    }
}
