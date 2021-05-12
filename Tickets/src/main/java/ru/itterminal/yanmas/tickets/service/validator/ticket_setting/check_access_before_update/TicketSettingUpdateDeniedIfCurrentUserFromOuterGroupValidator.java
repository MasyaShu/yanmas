package ru.itterminal.yanmas.tickets.service.validator.ticket_setting.check_access_before_update;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.tickets.model.TicketSetting;

@Component

public class TicketSettingUpdateDeniedIfCurrentUserFromOuterGroupValidator implements EntityValidator<TicketSetting> {
    @Override
    public void checkAccessBeforeUpdate(User currentUser) {
        throwAccessDeniedExceptionIfCurrentUserFromOuterGroup(currentUser);
    }
}
