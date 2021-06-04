package ru.itterminal.yanmas.tickets.service.validator.ticket_event.logical_validation;

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
public class FileMustHaveEmptyLinkToEntityBeforeCreateTicketEventValidator implements EntityValidator<TicketEvent> {
    public static final String FILE_ALREADY_HAS_A_LINK_TO_ANOTHER_ENTITY = "File %s already has a link to another entity";

    @Override
    public void logicalValidationBeforeCreate(TicketEvent entity, Map<String, List<ValidationError>> errors) {
        if (entity.getFiles() != null && !entity.getFiles().isEmpty()) {
            for (File file : entity.getFiles()) {
                if (file.getEntityId() != null) {
                    addValidationErrorIntoErrors(
                            FILE_IS_INVALID,
                            format(FILE_ALREADY_HAS_A_LINK_TO_ANOTHER_ENTITY, file.getFileName()),
                            errors
                    );
                }
            }
        }
    }
}
