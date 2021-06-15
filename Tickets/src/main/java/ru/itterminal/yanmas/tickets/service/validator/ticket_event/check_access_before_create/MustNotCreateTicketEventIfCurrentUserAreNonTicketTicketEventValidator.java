package ru.itterminal.yanmas.tickets.service.validator.ticket_event.check_access_before_create;

import lombok.RequiredArgsConstructor;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.tickets.model.TicketEvent;

import java.util.ArrayList;
import java.util.List;

@Component
@RequiredArgsConstructor
public class MustNotCreateTicketEventIfCurrentUserAreNonTicketTicketEventValidator implements EntityValidator<TicketEvent> {
    public static final String YOU_ARE_NOT_A_MEMBER_OF_THIS_TICKET =
            "You are not a member of this ticket";

    @Override
    public void checkAccessBeforeCreate(TicketEvent entity, User currentUser) {
        ArrayList<User> listUsersOfTicket = new ArrayList<>(List.of(entity.getTicket().getAuthor()));
        listUsersOfTicket.addAll(entity.getTicket().getExecutors());
        listUsersOfTicket.addAll(entity.getTicket().getObservers());
            if (!listUsersOfTicket.contains(currentUser)) {
                throw new AccessDeniedException(YOU_ARE_NOT_A_MEMBER_OF_THIS_TICKET);
            }

    }

}
