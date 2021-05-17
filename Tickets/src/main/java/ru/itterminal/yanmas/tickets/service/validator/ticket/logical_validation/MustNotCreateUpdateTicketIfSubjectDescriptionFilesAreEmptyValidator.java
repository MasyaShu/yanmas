package ru.itterminal.yanmas.tickets.service.validator.ticket.logical_validation;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.tickets.model.Ticket;

import java.util.List;
import java.util.Map;

import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

@Component
@RequiredArgsConstructor
public class MustNotCreateUpdateTicketIfSubjectDescriptionFilesAreEmptyValidator implements EntityValidator<Ticket> {
    public static final String MUST_NOT_CREATE_UPDATE_TICKET_IF_SUBJECT_DESCRIPTION_AND_FILES_ARE_EMPTY =
            "Mustn't create/update ticket if subject, description and files are empty";

    @Override
    public void logicalValidationBeforeCreate(Ticket entity, Map<String, List<ValidationError>> errors) {
        isEmptySubjectDescriptionAndFiles(entity, errors);
    }

    @Override
    public void logicalValidationBeforeUpdate(Ticket entity, Map<String, List<ValidationError>> errors) {
        isEmptySubjectDescriptionAndFiles(entity, errors);
    }

    private void isEmptySubjectDescriptionAndFiles(Ticket ticket, Map<String, List<ValidationError>> errors) {
        if ((ticket.getDescription() == null || ticket.getDescription().isEmpty())
                && (ticket.getSubject() == null || ticket.getSubject().isEmpty())
                && (ticket.getFiles() == null || ticket.getFiles().isEmpty())) {
            addValidationErrorIntoErrors(
                    EMPTY_TICKET,
                    MUST_NOT_CREATE_UPDATE_TICKET_IF_SUBJECT_DESCRIPTION_AND_FILES_ARE_EMPTY,
                    errors
            );
        }
    }
}
