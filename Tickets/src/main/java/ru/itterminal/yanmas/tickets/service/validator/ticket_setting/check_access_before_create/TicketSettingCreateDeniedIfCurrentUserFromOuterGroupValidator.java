package ru.itterminal.yanmas.tickets.service.validator.ticket_setting.check_access_before_create;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.tickets.model.TicketSetting;

@Component

public class TicketSettingCreateDeniedIfCurrentUserFromOuterGroupValidator implements EntityValidator<TicketSetting> {
    @Override
    public void checkAccessBeforeCreate(User currentUser) {
        throwAccessDeniedExceptionIfCurrentUserFromOuterGroup(currentUser);
    }
}
