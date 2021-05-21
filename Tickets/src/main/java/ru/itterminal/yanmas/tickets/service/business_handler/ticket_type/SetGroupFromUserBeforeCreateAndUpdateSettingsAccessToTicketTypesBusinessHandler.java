package ru.itterminal.yanmas.tickets.service.business_handler.ticket_type;

import org.springframework.stereotype.Component;

import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.tickets.model.SettingsAccessToTicketTypes;

@Component
public class SetGroupFromUserBeforeCreateAndUpdateSettingsAccessToTicketTypesBusinessHandler implements EntityBusinessHandler<SettingsAccessToTicketTypes> {

    @Override
    public SettingsAccessToTicketTypes beforeCreate(SettingsAccessToTicketTypes entity, User currentUser) {
        setGroupFromUser(entity);
        return entity;
    }

    @Override
    public SettingsAccessToTicketTypes beforeUpdate(SettingsAccessToTicketTypes entity, User currentUser) {
        setGroupFromUser(entity);
        return entity;
    }

    private void setGroupFromUser(SettingsAccessToTicketTypes entity) {
        if (entity.getUser() != null ) {
            entity.setGroup(entity.getUser().getGroup());
        }
    }
}
