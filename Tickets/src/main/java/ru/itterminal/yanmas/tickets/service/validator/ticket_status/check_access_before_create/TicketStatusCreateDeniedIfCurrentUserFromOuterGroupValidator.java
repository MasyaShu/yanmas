package ru.itterminal.yanmas.tickets.service.validator.ticket_status.check_access_before_create;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.tickets.model.TicketStatus;

@SuppressWarnings("unused")
@Component
public class TicketStatusCreateDeniedIfCurrentUserFromOuterGroupValidator implements EntityValidator<TicketStatus> {

    @Override
    public void checkAccessBeforeCreate(User currentUser) {
        throwAccessDeniedExceptionIfCurrentUserFromOuterGroup(currentUser);
    }
}
