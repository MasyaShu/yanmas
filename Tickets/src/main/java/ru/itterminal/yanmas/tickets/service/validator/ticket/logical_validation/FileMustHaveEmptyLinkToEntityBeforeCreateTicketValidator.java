package ru.itterminal.yanmas.tickets.service.validator.ticket.logical_validation;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.files.model.File;
import ru.itterminal.yanmas.tickets.model.Ticket;

import java.util.List;
import java.util.Map;

import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

@Component
public class FileMustHaveEmptyLinkToEntityBeforeCreateTicketValidator implements EntityValidator<Ticket> {
    public static final String FILE_IS_INVALID = "File is invalid";
    public static final String FILE_ALREADY_HAS_A_LINK_TO_ANOTHER_ENTITY = "File already has a link to another entity";

    @Override
    public void logicalValidationBeforeCreate(Ticket entity, Map<String, List<ValidationError>> errors) {
        if (entity.getFiles() != null && !entity.getFiles().isEmpty()) {
            for (File file : entity.getFiles()) {
                if (file.getEntityId() != null) {
                    addValidationErrorIntoErrors(
                            FILE_IS_INVALID,
                            FILE_ALREADY_HAS_A_LINK_TO_ANOTHER_ENTITY,
                            errors
                    );
                }
            }
        }
    }
}
