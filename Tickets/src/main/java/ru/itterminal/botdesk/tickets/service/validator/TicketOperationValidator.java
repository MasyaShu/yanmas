package ru.itterminal.botdesk.tickets.service.validator;

import static java.util.Collections.singletonList;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.ifErrorsNotEmptyThrowLogicalValidationException;

import java.util.List;
import java.util.Map;

import org.springframework.stereotype.Component;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.tickets.model.Ticket;

@Slf4j
@Component
public class TicketOperationValidator extends BasicOperationValidatorImpl<Ticket> {

    public static final String EMPTY_TICKET = "Empty ticket";
    public static final String LOG_EMPTY_TICKET = "Mustn't create/update ticket if subject, description and files are"
            + " empty: {}";
    public static final String MUST_NOT_CREATE_UPDATE_TICKET_IF_SUBJECT_DESCRIPTION_AND_FILES_ARE_EMPTY =
            "Mustn't create/update ticket if subject, description and files are empty";

    @Override
    public boolean beforeCreate(Ticket entity) {
        var result = super.beforeCreate(entity);
        var errors = createMapForLogicalErrors();
        IsEmptySubjectDescriptionAndFiles(entity, errors);
        ifErrorsNotEmptyThrowLogicalValidationException(errors);
        return result;
    }

    @Override
    public boolean beforeUpdate(Ticket entity) {
        var result = super.beforeUpdate(entity);
        var errors = createMapForLogicalErrors();
        IsEmptySubjectDescriptionAndFiles(entity, errors);
        ifErrorsNotEmptyThrowLogicalValidationException(errors);
        return result;
    }

    private void IsEmptySubjectDescriptionAndFiles(Ticket ticket, Map<String, List<ValidationError>> errors) {
        if ((ticket.getDescription() == null || ticket.getDescription().isEmpty())
                && (ticket.getSubject() == null || ticket.getSubject().isEmpty())
                && (ticket.getFiles() == null || ticket.getFiles().isEmpty())) {
            errors.put(
                    EMPTY_TICKET,
                    singletonList(new ValidationError(
                                          EMPTY_TICKET,
                                          MUST_NOT_CREATE_UPDATE_TICKET_IF_SUBJECT_DESCRIPTION_AND_FILES_ARE_EMPTY
                                  )
                    )
            );
            log.error(LOG_EMPTY_TICKET, ticket);
        }
    }
}
