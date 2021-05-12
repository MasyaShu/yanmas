package ru.itterminal.yanmas.aau.service.validator.user.check_access_before_read;

import static ru.itterminal.yanmas.aau.service.validator.user.check_access_before_read.AccessDeniedIfCurrentUserFromOuterGroupAndGroupsIdIsNotEqualsValidator.ACCESS_IS_DENIED_FOR_SEARCHING_BY_PASSED_ID;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;

import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;

@SuppressWarnings("unused")
@Component
public class AccessDeniedIfWeightOfRoleOfCurrentUserLessThanWeightOfExecutorAndGroupsIdIsNotEqualsValidator implements EntityValidator<User> {

    @SuppressWarnings("DuplicatedCode")
    @Override
    public void checkAccessBeforeRead(User entity, User currentUser) {
        if (currentUser != null) {
            var weightOfRoleOfCurrentUser = currentUser.getRole().getWeight();
            var idGroupFromEntity  = entity.getGroup().getId();
            var idGroupOfCurrentUser  = currentUser.getGroup().getId();
            if (weightOfRoleOfCurrentUser < Roles.EXECUTOR.getWeight()
                    && !idGroupFromEntity.equals(idGroupOfCurrentUser)) {
                throw new AccessDeniedException(ACCESS_IS_DENIED_FOR_SEARCHING_BY_PASSED_ID);
            }
        }
    }
}
