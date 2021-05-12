package ru.itterminal.yanmas.aau.service.validator.user.check_access_before_update;

import lombok.RequiredArgsConstructor;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;

import static java.lang.String.format;
import static ru.itterminal.yanmas.aau.service.validator.user.check_access_before_create.WeightOfRoleOfCurrentUserMustBeGreatThanWeightOfRoleFromEntityBeforeCreateUserValidator.IS_FORBIDDEN_TO_ASSIGN_A_USER_WITH_A_ROLE;

@Component
@RequiredArgsConstructor
public class WeightOfRoleOfCurrentUserMustBeGreatThanWeightOfRoleFromDatabaseBeforeUpdateUserValidator implements EntityValidator<User> {

    private final UserServiceImpl service;

    @Override
    public void checkAccessBeforeUpdate(User entity, User currentUser) {
        if (currentUser != null) {
            var userFromDatabase = service.findByIdAndAccountId(entity.getId(), currentUser.getAccount().getId());
            var weightOfRoleOfCurrentUser = currentUser.getRole().getWeight();
            var weightOfRoleFromDatabase = userFromDatabase.getRole().getWeight();
            if (weightOfRoleFromDatabase > weightOfRoleOfCurrentUser) {
                throw new AccessDeniedException(
                        format(
                                IS_FORBIDDEN_TO_ASSIGN_A_USER_WITH_A_ROLE,
                                userFromDatabase.getRole().getDisplayName()
                        ));
            }
        }
    }
}
