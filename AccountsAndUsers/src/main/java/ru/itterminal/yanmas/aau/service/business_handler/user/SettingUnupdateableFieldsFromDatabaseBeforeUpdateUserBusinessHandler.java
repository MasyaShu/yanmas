package ru.itterminal.yanmas.aau.service.business_handler.user;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;

@Component
@RequiredArgsConstructor
public class SettingUnupdateableFieldsFromDatabaseBeforeUpdateUserBusinessHandler implements EntityBusinessHandler<User> {

    private final UserServiceImpl userService;

    @Override
    public User beforeUpdate(User entity, User currentUser) {
        var entityFromDatabase = userService.findByIdAndAccountId(entity.getId(), currentUser);
        entity.setEmailVerificationStatus(entityFromDatabase.getEmailVerificationStatus());
        entity.setEmailVerificationToken(entityFromDatabase.getEmailVerificationToken());
        entity.setPasswordResetToken(entityFromDatabase.getPasswordResetToken());
        return entity;
    }

}
