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
public class CurrentUserRoleAuthorCanNotCreateUpdateTicketIfAuthorOfTicketIsNotCurrentUserValidator implements EntityValidator<Ticket> {
    public static final String
            CURRENT_USER_WITH_ROLE_AUTHOR_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_NOT_CURRENT_USER =
            "Current user with role AUTHOR can not create/update ticket if author of ticket is not current user";

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
                && !entity.getAuthor().equals(currentUser)) {
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_AUTHOR_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_NOT_CURRENT_USER);
        }

    }
}
