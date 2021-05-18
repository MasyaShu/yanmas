package ru.itterminal.yanmas.tickets.service.validator.ticket.check_access_before_read;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.tickets.model.Ticket;

@Component
public class CurrentUserRoleObserverCanNotReadTicketIfTicketHasNotCurrentUserInObserversValidator implements EntityValidator<Ticket> {
    public static final String
            CURRENT_USER_WITH_ROLE_OBSERVER_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_IN_OBSERVERS =
            "Current user with role OBSERVER can not read ticket if ticket has not current user in observers";

    @Override
    public void checkAccessBeforeRead(Ticket entity, User currentUser) {
        if (currentUser.getRole().getName().equals(Roles.OBSERVER.toString())
                && (entity.getObservers() == null || !entity.getObservers().contains(currentUser))) {
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_OBSERVER_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_IN_OBSERVERS);
        }
    }
}