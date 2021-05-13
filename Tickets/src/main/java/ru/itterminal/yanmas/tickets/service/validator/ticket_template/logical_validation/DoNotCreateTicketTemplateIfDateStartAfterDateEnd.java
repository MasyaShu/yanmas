package ru.itterminal.yanmas.tickets.service.validator.ticket_template.logical_validation;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.tickets.model.TicketTemplate;

import java.util.List;
import java.util.Map;

import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

@Component
public class DoNotCreateTicketTemplateIfDateStartAfterDateEnd implements EntityValidator<TicketTemplate> {

    public static final String THAN_DATE_END = "dateStart cannot be more than dateEnd";
    public static final String INVALID_TICKET_TEMPLATE = "Invalid ticket template";

    @Override
    public void logicalValidationBeforeCreate(TicketTemplate entity, Map<String, List<ValidationError>> errors) {
        checkDateStartAfterDateEnd(entity, errors);
    }

    @Override
    public void logicalValidationBeforeUpdate(TicketTemplate entity, Map<String, List<ValidationError>> errors) {
        EntityValidator.super.logicalValidationBeforeUpdate(entity, errors);
    }

    private void checkDateStartAfterDateEnd(TicketTemplate entity, Map<String, List<ValidationError>> errors) {
        if (entity.getDateStart() != null && entity.getDateEnd() != null && entity.getDateEnd() < entity.getDateStart()) {
            addValidationErrorIntoErrors(
                    INVALID_TICKET_TEMPLATE, THAN_DATE_END,
                    errors
            );
        }
    }
}
