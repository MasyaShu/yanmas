package ru.itterminal.yanmas.aau.service.validator.user.logical_validation_before_update;

import static ru.itterminal.yanmas.aau.service.validator.user.logical_validation_before_create.NotAllowedCreateMoreThanOneUserWithRoleAccountOwnerWithinAccountValidator.USER_WITH_ROLE_ACCOUNT_OWNER;
import static ru.itterminal.yanmas.aau.service.validator.user.logical_validation_before_create.NotAllowedCreateMoreThanOneUserWithRoleAccountOwnerWithinAccountValidator.USER_WITH_ROLE_ACCOUNT_OWNER_IS_OCCUPIED;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

import java.util.List;
import java.util.Map;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.repository.UserRepository;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;

@Component
@RequiredArgsConstructor
public class AfterUpdateCannotBeMoreThanOneUserWithRoleAccountOwnerWithinAccountValidator implements EntityValidator<User> {

    private final UserRepository repository;

    @Override
    public void logicalValidationBeforeCreate(User entity,
                                              Map<String, List<ValidationError>> errors) {
        var countUsersWithRoleAccountOwnerInDatabase =
                repository.countUserByRole_NameAndIdNot(Roles.ACCOUNT_OWNER.toString(), entity.getId());
        var isNewUserWithRoleAccountOwner = entity.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString());

        if (countUsersWithRoleAccountOwnerInDatabase != 0 && isNewUserWithRoleAccountOwner) {
            addValidationErrorIntoErrors(
                    USER_WITH_ROLE_ACCOUNT_OWNER,
                    USER_WITH_ROLE_ACCOUNT_OWNER_IS_OCCUPIED,
                    errors
            );
        }
    }
}
