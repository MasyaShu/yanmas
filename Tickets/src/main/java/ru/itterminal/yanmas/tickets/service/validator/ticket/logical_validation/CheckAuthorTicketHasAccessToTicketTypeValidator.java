package ru.itterminal.yanmas.tickets.service.validator.ticket.logical_validation;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.service.impl.SettingsAccessToTicketTypesServiceImpl;

import java.util.List;
import java.util.Map;

import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

@Component
@RequiredArgsConstructor
public class CheckAuthorTicketHasAccessToTicketTypeValidator implements EntityValidator<Ticket> {
    public static final String INVALID_TICKET = "Invalid ticket";
    public static final String INVALID_TICKET_BECAUSE_AUTHOR_HAS_NOT_ACCESS_TO_TICKET_TYPE =
            "Invalid ticket, because author has not access to ticket type";

    private final SettingsAccessToTicketTypesServiceImpl settingsAccessToTicketTypesService;

    @Override
    public void logicalValidationBeforeCreate(Ticket entity, Map<String, List<ValidationError>> errors) {
        isAuthorOfTicketHavePermitToTicketType(entity, errors);
    }

    @Override
    public void logicalValidationBeforeUpdate(Ticket entity, Map<String, List<ValidationError>> errors) {
        isAuthorOfTicketHavePermitToTicketType(entity, errors);
    }

    private void isAuthorOfTicketHavePermitToTicketType(Ticket ticket, Map<String, List<ValidationError>> errors) {
        if (ticket.getTicketType() != null && ticket.getAuthor() != null) {
            var authorId = ticket.getAuthor().getId();
            var ticketTypeId = ticket.getTicketType().getId();
            if (!settingsAccessToTicketTypesService.isPermittedTicketType(ticketTypeId, authorId)) {
                addValidationErrorIntoErrors(
                        INVALID_TICKET, INVALID_TICKET_BECAUSE_AUTHOR_HAS_NOT_ACCESS_TO_TICKET_TYPE,
                        errors
                );
            }
        }
    }
}
