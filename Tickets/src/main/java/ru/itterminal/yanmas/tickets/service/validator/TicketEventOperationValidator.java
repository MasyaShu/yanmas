package ru.itterminal.yanmas.tickets.service.validator;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.service.validator.BasicOperationValidatorWithCurrentUserImpl;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.tickets.model.TicketEvent;

import java.util.List;
import java.util.Map;

import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.*;

@Slf4j
@Component
@RequiredArgsConstructor
public class TicketEventOperationValidator extends BasicOperationValidatorWithCurrentUserImpl<TicketEvent> {

    public static final String MUST_NOT_CREATE_EVENT_IF_FIELDS_TICKET_CREATED_BY_OR_CREATED_AT_ARE_EMPTY =
            "Mustn't create event if fields 'ticket', 'createdBy' or 'createdAt' are empty";
    public static final String MUST_NOT_CREATE_EVENT_IF_FIELDS_COMMENT_AUTO_COMMENT_AND_FILES_ARE_EMPTY =
            "Mustn't create event if fields 'comment', 'autoComment' and 'files' are empty";
    public static final String EMPTY_FIELDS = "Empty fields";

    @Override
    public boolean logicalValidationBeforeCreate(TicketEvent entity) {
        var result = super.logicalValidationBeforeUpdate(entity);
        var errors = createMapForLogicalErrors();
        isEmptyTicketOrCreatedAtOrCreatedBy(entity, errors);
        isEmptyCommentAutoCommentAndFiles(entity, errors);
        ifErrorsNotEmptyThrowLogicalValidationException(errors);
        return result;
    }

    void isEmptyTicketOrCreatedAtOrCreatedBy(TicketEvent event, Map<String, List<ValidationError>> errors) {
        if (event.getTicket() == null
                || event.getCreatedAt() == null
                || event.getCreatedBy() == null) {
            addValidationErrorIntoErrors(
                    EMPTY_FIELDS,
                    MUST_NOT_CREATE_EVENT_IF_FIELDS_TICKET_CREATED_BY_OR_CREATED_AT_ARE_EMPTY,
                    errors
            );
        }
    }

    void isEmptyCommentAutoCommentAndFiles(TicketEvent event, Map<String, List<ValidationError>> errors) {
        if ((event.getComment() == null || event.getComment().isEmpty())
                && (event.getAutoComment() == null || event.getAutoComment().isEmpty())
                && (event.getFiles() == null || event.getFiles().isEmpty())) {
            addValidationErrorIntoErrors(
                    EMPTY_FIELDS,
                    MUST_NOT_CREATE_EVENT_IF_FIELDS_COMMENT_AUTO_COMMENT_AND_FILES_ARE_EMPTY,
                    errors
            );
        }
    }
}
