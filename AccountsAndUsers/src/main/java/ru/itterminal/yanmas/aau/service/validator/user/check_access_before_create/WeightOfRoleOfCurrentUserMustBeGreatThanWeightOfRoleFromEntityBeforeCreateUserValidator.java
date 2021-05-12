package ru.itterminal.yanmas.aau.service.validator.user.check_access_before_create;

import lombok.RequiredArgsConstructor;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;

import static java.lang.String.format;

@Component
public class WeightOfRoleOfCurrentUserMustBeGreatThanWeightOfRoleFromEntityBeforeCreateUserValidator implements EntityValidator<User> {

    public static final String IS_FORBIDDEN_TO_ASSIGN_A_USER_WITH_A_ROLE =
            "With your role it is forbidden to assign a user with a role %s";


    @Override
    public void checkAccessBeforeCreate(User entity, User currentUser) {
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
