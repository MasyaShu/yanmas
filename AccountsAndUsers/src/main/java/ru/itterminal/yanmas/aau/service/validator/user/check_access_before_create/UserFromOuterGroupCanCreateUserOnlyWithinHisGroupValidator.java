package ru.itterminal.yanmas.aau.service.validator.user.check_access_before_create;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;

import static java.lang.String.format;

@Component
public class UserFromOuterGroupCanCreateUserOnlyWithinHisGroupValidator implements EntityValidator<User> {

    public static final String NOT_ALLOWED_TO_ASSIGN_A_GROUP_WITH_ID_NOT_EQUAL =
            "User from outer group are not allowed to assign a group with id not equal: %s";

    @SuppressWarnings("DuplicatedCode")
    @Override
    public void checkAccessBeforeCreate(User entity, User currentUser) {
        if (currentUser != null) {
            var groupOfCurrentUser = currentUser.getGroup();
            var groupFromEntity = entity.getGroup();
            if (Boolean.FALSE.equals(groupOfCurrentUser.getIsInner())
                    && !groupFromEntity.getId().equals(groupOfCurrentUser.getId())) {
                throw new AccessDeniedException(format(
                        NOT_ALLOWED_TO_ASSIGN_A_GROUP_WITH_ID_NOT_EQUAL,
                        currentUser.getGroup().getId()
                ));
            }
        }
    }
}
