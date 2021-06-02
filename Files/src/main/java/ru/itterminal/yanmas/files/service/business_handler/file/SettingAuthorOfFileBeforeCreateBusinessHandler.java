package ru.itterminal.yanmas.files.service.business_handler.file;

import org.springframework.stereotype.Component;

import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.files.model.File;

@SuppressWarnings("unused")
@Component
public class SettingAuthorOfFileBeforeCreateBusinessHandler implements EntityBusinessHandler<File> {

    @Override
    public File beforeCreate(File entity, User currentUser) {
        entity.setAuthorId(currentUser.getId());
        return entity;
    }
}
