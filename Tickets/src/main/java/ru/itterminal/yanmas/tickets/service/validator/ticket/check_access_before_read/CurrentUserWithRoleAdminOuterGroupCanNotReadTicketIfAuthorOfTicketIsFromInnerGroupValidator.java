package ru.itterminal.yanmas.tickets.service.validator.ticket.check_access_before_read;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.tickets.model.Ticket;

@Component
public class CurrentUserWithRoleAdminOuterGroupCanNotReadTicketIfAuthorOfTicketIsFromInnerGroupValidator implements EntityValidator<Ticket> {
    public static final String
            CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP =
            "Current user with role ADMIN from outer group can not read ticket if author of ticket is from inner group";

    @Override
    public void checkAccessBeforeRead(Ticket entity, User currentUser) {
        if (currentUser.getRole().getName().equals(Roles.ADMIN.toString())
                && Boolean.FALSE.equals(currentUser.getGroup().getIsInner())
                && Boolean.TRUE.equals(entity.getAuthor().getGroup().getIsInner())) {
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP);
        }
    }
}
