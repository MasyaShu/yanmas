package ru.itterminal.yanmas.tickets.service.validator.ticket_event.logical_validation;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.tickets.model.TicketEvent;

import java.util.List;
import java.util.Map;

import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

@Component
@RequiredArgsConstructor
public class CheckIsEmptyFieldsCreatedByInEventValidator implements EntityValidator<TicketEvent> {
    public static final String MUST_NOT_CREATE_EVENT_IF_FIELDS_CREATED_AT_ARE_EMPTY =
            "Mustn't create event if fields 'createdBy' are empty";

    @Override
    public void logicalValidationBeforeCreate(TicketEvent entity, Map<String, List<ValidationError>> errors) {
        if (entity.getCreatedBy() == null) {
            addValidationErrorIntoErrors(
                    EMPTY_FIELDS,
                    MUST_NOT_CREATE_EVENT_IF_FIELDS_CREATED_AT_ARE_EMPTY,
                    errors
            );
        }
    }
}
