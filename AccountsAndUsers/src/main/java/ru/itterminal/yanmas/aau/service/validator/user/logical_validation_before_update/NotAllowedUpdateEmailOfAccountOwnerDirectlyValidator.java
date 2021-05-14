package ru.itterminal.yanmas.aau.service.validator.user.logical_validation_before_update;

import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

import java.util.List;
import java.util.Map;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;

@Component
@RequiredArgsConstructor
public class NotAllowedUpdateEmailOfAccountOwnerDirectlyValidator implements EntityValidator<User> {

    private final UserServiceImpl service;
    public static final String USER_WITH_ROLE_ACCOUNT_OWNER = "User with role ACCOUNT_OWNER";
    public static final String YOU_CAN_EDIT_EMAIL_OF_THE_USER_WITH_A_ROLE_ACCOUNT_OWNER_ONLY_VIA_SPECIAL_PROCESS =
            "You can edit email of the user with a role Account owner only via special process called \"update email of account owner\"";

    @Override
    public void logicalValidationBeforeUpdate(User entity,
                                              Map<String, List<ValidationError>> errors) {
        var isNewUserWithRoleAccountOwner = entity.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString());
        var userFromDatabase = service.findById(entity.getId());
        if (isNewUserWithRoleAccountOwner && !entity.getEmail().equals(userFromDatabase.getEmail())) {
            addValidationErrorIntoErrors(
                    USER_WITH_ROLE_ACCOUNT_OWNER,
                    YOU_CAN_EDIT_EMAIL_OF_THE_USER_WITH_A_ROLE_ACCOUNT_OWNER_ONLY_VIA_SPECIAL_PROCESS,
                    errors
            );
        }
    }
}
