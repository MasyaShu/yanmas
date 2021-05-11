package ru.itterminal.yanmas.tickets.service.validator.ticket_status;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.tickets.model.TicketStatus;

@SuppressWarnings("unused")
@Component
public class CheckAccessBeforeUpdateTicketStatusAccessDeniedForUsersFromOuterGroups implements EntityValidator<TicketStatus> {

    @Override
    public void checkAccessBeforeUpdate(User currentUser) {
        throwAccessDeniedExceptionIfCurrentUserFromOuterGroup(currentUser);
    }
}
