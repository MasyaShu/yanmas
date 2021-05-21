package ru.itterminal.yanmas.tickets.service.validator.ticket.check_access_before_read;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.tickets.model.Ticket;

@Component
public class CurrentUserWithRoleAuthorFromOuterGroupCanNotReadTicketIfTicketFromAnotherGroupValidator implements EntityValidator<Ticket> {
    public static final String
            CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_OUTER_GROUP =
            "Current user with role AUTHOR can not read ticket if author of ticket is from another group";

    @Override
    public void checkAccessBeforeRead(Ticket entity, User currentUser) {
        if (currentUser.getRole().getName().equals(Roles.AUTHOR.toString())
                && !currentUser.getGroup().equals(entity.getGroup())) {
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_OUTER_GROUP);
        }
    }
}
