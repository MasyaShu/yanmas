package ru.itterminal.yanmas.aau.service.validator.user.logical_validation_before_create;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.repository.UserRepository;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;

import java.util.List;
import java.util.Map;

import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

@Component
@RequiredArgsConstructor
public class NotAllowedCreateMoreThanOneUserWithRoleAccountOwnerWithinAccountValidator implements EntityValidator<User> {

    public static final String USER_WITH_ROLE_ACCOUNT_OWNER_IS_OCCUPIED = "User with role ACCOUNT_OWNER is occupied";
    public static final String USER_WITH_ROLE_ACCOUNT_OWNER = "User with role ACCOUNT_OWNER";

    private final UserRepository repository;

    @Override
    public void logicalValidationBeforeCreate(User entity,
                                              Map<String, List<ValidationError>> errors) {
        var isNewUserWithRoleAccountOwner = entity.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString());
        var countUsersWithRoleAccountOwner =
                repository.countUserByRole_NameAndAccount_Id(Roles.ACCOUNT_OWNER.toString(), entity.getAccount().getId());
        if (isNewUserWithRoleAccountOwner && countUsersWithRoleAccountOwner != 0) {
            addValidationErrorIntoErrors(
                    USER_WITH_ROLE_ACCOUNT_OWNER,
                    USER_WITH_ROLE_ACCOUNT_OWNER_IS_OCCUPIED,
                    errors
            );
        }
    }
}
