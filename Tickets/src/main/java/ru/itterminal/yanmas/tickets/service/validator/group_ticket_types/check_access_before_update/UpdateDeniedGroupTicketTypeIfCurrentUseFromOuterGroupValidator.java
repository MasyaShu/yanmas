package ru.itterminal.yanmas.tickets.service.validator.group_ticket_types.check_access_before_update;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.tickets.model.GroupTicketTypes;

@Component
public class UpdateDeniedGroupTicketTypeIfCurrentUseFromOuterGroupValidator implements EntityValidator<GroupTicketTypes> {
    @Override
    public void checkAccessBeforeUpdate(User currentUser) {
            throwAccessDeniedExceptionIfCurrentUserFromOuterGroup(currentUser);
    }
}
