package ru.itterminal.yanmas.aau.service.validator.property_group.check_access_before_create;

import org.springframework.stereotype.Component;

import ru.itterminal.yanmas.aau.model.PropertyGroup;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;

@SuppressWarnings("unused")
@Component
public class AccessDeniedIfCurrentUserFromOuterGroupBeforeCreatePropertyGroupValidator implements EntityValidator<PropertyGroup> {
    @Override
    public void checkAccessBeforeCreate(User currentUser) {
        if (currentUser != null) {
            throwAccessDeniedExceptionIfCurrentUserFromOuterGroup(currentUser);
        }
    }
}
