package ru.itterminal.yanmas.tickets.service.validator.ticket_event.logical_validation;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.tickets.model.TicketEvent;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

@Component
@RequiredArgsConstructor
public class MustNotCreateTicketEventIfRecipientsAreNonTicketTicketEventValidator implements EntityValidator<TicketEvent> {
    public static final String TICKET_EVENT_IS_INVALID = "Ticket event is invalid";
    public static final String RECIPIENT_NOT_FOUND_IN_THE_TICKET =
            "Recipient %s not found in the ticket";

    @Override
    public void logicalValidationBeforeCreate(TicketEvent entity, Map<String, List<ValidationError>> errors) {
        if (entity.getTicket() != null
                && entity.getRecipients() != null
                && !entity.getRecipients().isEmpty()) {
            ArrayList<User> listUsersOfTicket = new ArrayList<>(List.of(entity.getTicket().getAuthor()));
            listUsersOfTicket.addAll(entity.getTicket().getExecutors());
            listUsersOfTicket.addAll(entity.getTicket().getObservers());
            for (User user : entity.getRecipients()) {
                if (!listUsersOfTicket.contains(user)) {
                    addValidationErrorIntoErrors(
                            TICKET_EVENT_IS_INVALID,
                            format(RECIPIENT_NOT_FOUND_IN_THE_TICKET, user.getDisplayName()),
                            errors
                    );
                }
            }
        }
    }
}
