package ru.itterminal.yanmas.aau.service.business_handler.user;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.security.jwt.JwtProvider;

@Component
@RequiredArgsConstructor
public class SettingEmailVerificationTokenBeforeCreateUserBusinessHandler implements EntityBusinessHandler<User> {

    private final JwtProvider jwtProvider;

    @Override
    public User beforeCreate(User entity, User currentUser) {
        if (entity.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString())) {
            var emailVerificationToken = jwtProvider.createTokenWithUserId(entity.getId());
            entity.setEmailVerificationToken(emailVerificationToken);
            entity.setEmailVerificationStatus(false);
        } else {
            entity.setEmailVerificationStatus(true);
        }
        return entity;
    }
}
