package ru.itterminal.botdesk.tickets.service.validator;

import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.commons.util.CommonMethodsForValidation;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;

public class TicketTemplateOperationValidator extends BasicOperationValidatorImpl<TicketTemplate> {
    public static final String THAN_DATE_END = "dateStart cannot be more than dateEnd";

    @Override
    public boolean beforeCreate(TicketTemplate entity) {
        checkDateStartAfterDateEnd(entity);
        return super.beforeCreate(entity);
    }

    @Override
    public boolean beforeUpdate(TicketTemplate entity) {
        checkDateStartAfterDateEnd(entity);
        return super.beforeUpdate(entity);
    }

    private void checkDateStartAfterDateEnd(TicketTemplate entity) {
        if(entity.getDateStart() != null && entity.getDateEnd() != null && entity.getDateEnd() < entity.getDateStart()) {
            throw CommonMethodsForValidation.createExpectedLogicalValidationException(VALIDATION_FAILED, THAN_DATE_END);
        }
    }

}
