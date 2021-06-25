package ru.itterminal.yanmas.aau.service.validator.property_values.check_access_before_create_update_read;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.PropertyValues;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.aau.util.ReflectionHelper;

@Component
@RequiredArgsConstructor
public class AccessDeniedIfCurrentUserNotAccessForEntityValidator implements EntityValidator<PropertyValues> {

    private final ReflectionHelper reflectionHelper;

    @Override
    public void checkAccessBeforeRead(PropertyValues entity, User currentUser) {
        checkAccessCurrentUserForEntity(entity, currentUser);
    }

    @Override
    public void checkAccessBeforeCreate(PropertyValues entity, User currentUser) {
        checkAccessCurrentUserForEntity(entity, currentUser);
    }

    @Override
    public void checkAccessBeforeUpdate(PropertyValues entity, User currentUser) {
        checkAccessCurrentUserForEntity(entity, currentUser);
    }

    private void checkAccessCurrentUserForEntity(PropertyValues entity, User currentUser) {
        reflectionHelper.checkAccessForReadEntityByIdAndNameClass
                (entity.getProperty().getEntityName(),
                        entity.getEntityId(),
                        currentUser);
    }
}
