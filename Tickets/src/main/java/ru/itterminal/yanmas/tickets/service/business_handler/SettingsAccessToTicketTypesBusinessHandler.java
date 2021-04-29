package ru.itterminal.yanmas.tickets.service.business_handler;

import org.springframework.stereotype.Component;

import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.tickets.model.SettingsAccessToTicketTypes;

@Component
public class SettingsAccessToTicketTypesBusinessHandler implements EntityBusinessHandler<SettingsAccessToTicketTypes> {

    @Override
    public void beforeCreate(SettingsAccessToTicketTypes entity, User currentUser) {
        if (entity.getUser() != null ) {
            entity.setGroup(entity.getUser().getGroup());
        }
    }

    @Override
    public void beforeUpdate(SettingsAccessToTicketTypes entity, User currentUser) {
        beforeCreate(entity, currentUser);
    }
}
