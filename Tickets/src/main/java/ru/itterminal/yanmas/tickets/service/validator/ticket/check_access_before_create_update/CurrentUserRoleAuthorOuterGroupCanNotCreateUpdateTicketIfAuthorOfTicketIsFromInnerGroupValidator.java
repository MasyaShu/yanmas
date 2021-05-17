package ru.itterminal.yanmas.tickets.service.validator.ticket.check_access_before_create_update;

import lombok.RequiredArgsConstructor;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.tickets.model.Ticket;

@Component
@RequiredArgsConstructor
public class CurrentUserRoleAuthorOuterGroupCanNotCreateUpdateTicketIfAuthorOfTicketIsFromInnerGroupValidator implements EntityValidator<Ticket> {
    public static final String
            CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP =
            "Current user with role AUTHOR from outer group can not create/update ticket if author of ticket is from inner group";

    @Override
    public void checkAccessBeforeCreate(Ticket entity, User currentUser) {
        checkAccessForCreateAndUpdate(entity, currentUser);
    }

    @Override
    public void checkAccessBeforeUpdate(Ticket entity, User currentUser) {
        checkAccessForCreateAndUpdate(entity, currentUser);
    }

    private void checkAccessForCreateAndUpdate(Ticket entity, User currentUser) {
        if (currentUser.getRole().getName().equals(Roles.AUTHOR.toString())
                && Boolean.FALSE.equals(currentUser.getGroup().getIsInner())
                && Boolean.TRUE.equals(entity.getAuthor().getGroup().getIsInner())) {
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP);
        }
    }
}
