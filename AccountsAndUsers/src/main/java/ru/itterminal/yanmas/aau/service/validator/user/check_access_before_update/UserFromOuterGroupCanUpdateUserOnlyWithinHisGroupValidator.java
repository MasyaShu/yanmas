package ru.itterminal.yanmas.aau.service.validator.user.check_access_before_update;

import lombok.RequiredArgsConstructor;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;

import static java.lang.String.format;
import static ru.itterminal.yanmas.aau.service.validator.user.check_access_before_create.UserFromOuterGroupCanCreateUserOnlyWithinHisGroupValidator.NOT_ALLOWED_TO_ASSIGN_A_GROUP_WITH_ID_NOT_EQUAL;

@Component
@RequiredArgsConstructor
public class UserFromOuterGroupCanUpdateUserOnlyWithinHisGroupValidator implements EntityValidator<User> {

    private final UserServiceImpl service;

    @SuppressWarnings("DuplicatedCode")
    @Override
    public void checkAccessBeforeUpdate(User entity, User currentUser) {
        if (currentUser != null) {
            var groupOfCurrentUser = currentUser.getGroup();
            var groupFromEntity = entity.getGroup();
            if (Boolean.FALSE.equals(groupOfCurrentUser.getIsInner())) {
                if (!groupFromEntity.getId().equals(groupOfCurrentUser.getId())) {
                    throw new AccessDeniedException(
                            format(
                                    NOT_ALLOWED_TO_ASSIGN_A_GROUP_WITH_ID_NOT_EQUAL,
                                    currentUser.getGroup().getId()
                            )
                    );
                }
                var userFromDatabase = service.findById(entity.getId());
                var groupFromDataBase = userFromDatabase.getGroup();
                if (!groupFromDataBase.getId().equals(groupOfCurrentUser.getId())) {
                    throw new AccessDeniedException(
                            format(
                                    NOT_ALLOWED_TO_ASSIGN_A_GROUP_WITH_ID_NOT_EQUAL,
                                    currentUser.getGroup().getId()
                            )
                    );
                }
            }

        }
    }
}
