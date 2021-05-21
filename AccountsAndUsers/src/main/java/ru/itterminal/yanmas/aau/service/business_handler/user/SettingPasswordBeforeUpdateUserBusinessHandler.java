package ru.itterminal.yanmas.aau.service.business_handler.user;

import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;

@Component
@RequiredArgsConstructor
public class SettingPasswordBeforeUpdateUserBusinessHandler implements EntityBusinessHandler<User> {

    private final BCryptPasswordEncoder encoder;
    private final UserServiceImpl userService;

    @Override
    public User beforeUpdate(User entity, User currentUser) {
        var entityFromDatabase = userService.findByIdAndAccountId(entity.getId(), currentUser);
        if (entity.getPassword() == null || entity.getPassword().isEmpty()) {
            entity.setPassword(entityFromDatabase.getPassword());
        } else {
            entity.setPassword(encoder.encode(entity.getPassword()));
        }
        return entity;
    }

}
