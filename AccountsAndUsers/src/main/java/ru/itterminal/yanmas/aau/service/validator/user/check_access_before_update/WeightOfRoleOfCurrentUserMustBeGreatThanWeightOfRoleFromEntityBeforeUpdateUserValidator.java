package ru.itterminal.yanmas.aau.service.validator.user.check_access_before_update;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;

import static java.lang.String.format;
import static ru.itterminal.yanmas.aau.service.validator.user.check_access_before_create.WeightOfRoleOfCurrentUserMustBeGreatThanWeightOfRoleFromEntityBeforeCreateUserValidator.IS_FORBIDDEN_TO_ASSIGN_A_USER_WITH_A_ROLE;

@Component
public class WeightOfRoleOfCurrentUserMustBeGreatThanWeightOfRoleFromEntityBeforeUpdateUserValidator implements EntityValidator<User> {

    @Override
    public void checkAccessBeforeUpdate(User entity, User currentUser) {
        if (currentUser != null) {
            var weightOfRoleOfCurrentUser = currentUser.getRole().getWeight();
            var weightOfRoleFromEntity = entity.getRole().getWeight();
            if (weightOfRoleFromEntity > weightOfRoleOfCurrentUser) {
                throw new AccessDeniedException(
                        format(
                                IS_FORBIDDEN_TO_ASSIGN_A_USER_WITH_A_ROLE,
                                entity.getRole().getDisplayName()
                        ));
            }
        }
    }
}
