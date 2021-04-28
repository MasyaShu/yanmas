package ru.itterminal.yanmas.tickets.service.business_handler;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.aau.service.impl.GroupServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.tickets.model.SettingsAccessToTicketTypes;
import ru.itterminal.yanmas.tickets.service.impl.GroupTicketTypesServiceImpl;

@Component
@RequiredArgsConstructor
public class SettingsAccessToTicketTypesBusinessHandler implements EntityBusinessHandler<SettingsAccessToTicketTypes> {

    private final GroupServiceImpl groupService;
    private final UserServiceImpl userService;
    private final GroupTicketTypesServiceImpl groupTicketTypesService;

    @Override
    public void beforeCreate(SettingsAccessToTicketTypes entity, User currentUser) {
        entity.setAccount(currentUser.getAccount());
        if (entity.getUser() != null && entity.getUser().getId() != null) {
            entity.setUser(userService.findByIdAndAccountId(entity.getUser().getId()));
            entity.setGroup(entity.getUser().getGroup());
        } else if (entity.getGroup() != null && entity.getGroup().getId() != null) {
            entity.setGroup(groupService.findByIdAndAccountId(entity.getGroup().getId()));
        }
        if (entity.getGroupTicketTypes() != null && entity.getGroupTicketTypes().getId() != null) {
            entity.setGroupTicketTypes(
                    groupTicketTypesService.findByIdAndAccountId(entity.getGroupTicketTypes().getId()));
        }
    }

    @Override
    public void beforeUpdate(SettingsAccessToTicketTypes entity, User currentUser) {
        beforeCreate(entity, currentUser);
    }
}
