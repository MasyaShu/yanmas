package ru.itterminal.yanmas.aau.service.business_handler.user;

import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;

@Component
@RequiredArgsConstructor
public class SettingEncodedPasswordBeforeCreateUserBusinessHandler implements EntityBusinessHandler<User> {

    private final BCryptPasswordEncoder encoder;

    @Override
    public User beforeCreate(User entity, User currentUser) {
        entity.setPassword(encoder.encode(entity.getPassword()));
        return entity;
    }
}
