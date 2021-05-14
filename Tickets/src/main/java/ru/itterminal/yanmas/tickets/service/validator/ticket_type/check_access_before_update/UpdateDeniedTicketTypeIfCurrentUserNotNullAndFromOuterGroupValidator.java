package ru.itterminal.yanmas.tickets.service.validator.ticket_type.check_access_before_update;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.tickets.model.TicketType;

@Component
public class UpdateDeniedTicketTypeIfCurrentUserNotNullAndFromOuterGroupValidator implements EntityValidator<TicketType> {
    @Override
    public void checkAccessBeforeUpdate(User currentUser) {
        if (currentUser != null) {
            throwAccessDeniedExceptionIfCurrentUserFromOuterGroup(currentUser);
        }
    }
}
