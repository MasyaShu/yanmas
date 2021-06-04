package ru.itterminal.yanmas.tickets.service.validator.ticket_event.logical_validation;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.files.model.File;
import ru.itterminal.yanmas.tickets.model.TicketEvent;

import java.util.List;
import java.util.Map;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

@Component
@RequiredArgsConstructor
public class MustNotCreateTicketEventIfFileWasCreatedByAnotherUserTicketEventValidator implements EntityValidator<TicketEvent> {
    public static final String
            FILE_WAS_CREATED_BY_ANOTHER_USER_YOU_CANNOT_USE_IT_FOR_CREATE_THIS_TICKET_EVENT =
            "File %s was created by another user, you cannot use it for create this ticket event";

    @Override
    public void logicalValidationBeforeCreate(TicketEvent entity, Map<String, List<ValidationError>> errors) {
        if (entity.getFiles() != null && !entity.getFiles().isEmpty()) {
            for (File file : entity.getFiles()) {
                if (!file.getAuthorId().equals(entity.getCreatedBy().getId())) {
                    addValidationErrorIntoErrors(
                            FILE_IS_INVALID,
                            format(FILE_WAS_CREATED_BY_ANOTHER_USER_YOU_CANNOT_USE_IT_FOR_CREATE_THIS_TICKET_EVENT, file.getFileName()),
                            errors
                    );
               }
            }
        }
    }
}
