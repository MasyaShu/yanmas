package ru.itterminal.yanmas.aau.service.validator.user.logical_validation_before_update;

import static ru.itterminal.yanmas.aau.service.validator.user.logical_validation_before_create.NotAllowedCreateMoreThanOneUserWithRoleAccountOwnerWithinAccountValidator.USER_WITH_ROLE_ACCOUNT_OWNER;
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
public class AfterUpdateMustBeOneUserWithRoleAccountOwnerWithinAccountValidator implements EntityValidator<User> {

    private final UserRepository repository;

    public static final String AFTER_UPDATE_MUST_BE_ONE_USER_WITH_ROLE_ACCOUNT_OWNER =
            "After update must be one user with role AccountOwner";

    @Override
    public void logicalValidationBeforeCreate(User entity,
                                              Map<String, List<ValidationError>> errors) {
        var countUsersWithRoleAccountOwnerInDatabase =
                repository.countUserByRole_NameAndAccount_IdAndIdNot(Roles.ACCOUNT_OWNER.toString(),
                                                                     entity.getAccount().getId(),
                                                                     entity.getId());
        var isNewUserWithRoleAccountOwner = entity.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString());

        if (countUsersWithRoleAccountOwnerInDatabase == 0 && !isNewUserWithRoleAccountOwner) {
            addValidationErrorIntoErrors(
                    USER_WITH_ROLE_ACCOUNT_OWNER,
                    AFTER_UPDATE_MUST_BE_ONE_USER_WITH_ROLE_ACCOUNT_OWNER,
                    errors
            );
        }
    }

}
