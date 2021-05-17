package ru.itterminal.yanmas.tickets.service.validator.ticket.check_access_before_create_update;

import lombok.RequiredArgsConstructor;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.service.impl.SettingsAccessToTicketTypesServiceImpl;

@Component
@RequiredArgsConstructor
public class CurrentUserCanNotCreateUpdateTicketIfNotPermitToTicketTypeValidator implements EntityValidator<Ticket> {
    public static final String ACCESS_DENIED_BECAUSE_CURRENT_USER_HAS_NOT_PERMIT_TO_TICKET_TYPE =
            "Access denied, because current user has not permit to ticket type";

    private final SettingsAccessToTicketTypesServiceImpl settingsAccessToTicketTypesService;

    @Override
    public void checkAccessBeforeCreate(Ticket entity, User currentUser) {
        checkAccessForCreateAndUpdate(entity, currentUser);
    }

    @Override
    public void checkAccessBeforeUpdate(Ticket entity, User currentUser) {
        checkAccessForCreateAndUpdate(entity, currentUser);
    }

    private void checkAccessForCreateAndUpdate(Ticket entity, User currentUser) {
        if (entity.getTicketType() != null) {
            var ticketTypeId = entity.getTicketType().getId();
            var currentUserId = currentUser.getId();
            if (!settingsAccessToTicketTypesService.isPermittedTicketType(ticketTypeId, currentUserId)) {
                throw new AccessDeniedException(ACCESS_DENIED_BECAUSE_CURRENT_USER_HAS_NOT_PERMIT_TO_TICKET_TYPE);
            }
        }
    }
}
