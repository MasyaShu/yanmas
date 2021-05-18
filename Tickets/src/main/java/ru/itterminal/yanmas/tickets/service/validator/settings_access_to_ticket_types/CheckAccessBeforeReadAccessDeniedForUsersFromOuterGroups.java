package ru.itterminal.yanmas.tickets.service.validator.settings_access_to_ticket_types;

import org.springframework.stereotype.Component;

import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.tickets.model.SettingsAccessToTicketTypes;

@SuppressWarnings("unused")
@Component
public class CheckAccessBeforeReadAccessDeniedForUsersFromOuterGroups implements EntityValidator<SettingsAccessToTicketTypes> {

    @Override
    public void checkAccessBeforeRead(User currentUser) {
        throwAccessDeniedExceptionIfCurrentUserFromOuterGroup(currentUser);
    }
}