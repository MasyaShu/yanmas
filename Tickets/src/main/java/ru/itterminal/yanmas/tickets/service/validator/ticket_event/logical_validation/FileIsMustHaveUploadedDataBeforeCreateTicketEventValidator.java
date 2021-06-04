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
public class FileIsMustHaveUploadedDataBeforeCreateTicketEventValidator implements EntityValidator<TicketEvent> {
        public static final String FILE_IS_NOT_YET_UPLOADED = "File %s is not yet uploaded";

    @Override
    public void logicalValidationBeforeCreate(TicketEvent entity, Map<String, List<ValidationError>> errors) {
        if (entity.getFiles() != null && !entity.getFiles().isEmpty()) {
            for (File file : entity.getFiles()) {
                if (Boolean.FALSE.equals(file.getIsUploaded())) {
                    addValidationErrorIntoErrors(
                            FILE_IS_INVALID,
                            format(FILE_IS_NOT_YET_UPLOADED, file.getFileName()),
                            errors
                    );
                }
            }
        }
    }
}
