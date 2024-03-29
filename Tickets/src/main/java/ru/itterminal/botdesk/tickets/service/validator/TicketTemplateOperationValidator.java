package ru.itterminal.botdesk.tickets.service.validator;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.commons.util.CommonMethodsForValidation;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;

public class TicketTemplateOperationValidator extends BasicOperationValidatorImpl<TicketTemplate> {
    public static final String THAN_DATE_END = "dateStart cannot be more than dateEnd";
    public static final String A_USER_FROM_NOT_INNER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_TEMPLATE = "A user from not inner group cannot create or update ticket template";

    @Override
    public boolean beforeCreate(TicketTemplate entity) {
        checkIsInnerGroupForCreateUpdate();
        checkDateStartAfterDateEnd(entity);
        return super.beforeCreate(entity);
    }

    @Override
    public boolean beforeUpdate(TicketTemplate entity) {
        checkIsInnerGroupForCreateUpdate();
        checkDateStartAfterDateEnd(entity);
        return super.beforeUpdate(entity);
    }

    private void checkDateStartAfterDateEnd(TicketTemplate entity) {
        if(entity.getDateStart() != null && entity.getDateEnd() != null && entity.getDateEnd() < entity.getDateStart()) {
            throw CommonMethodsForValidation.createExpectedLogicalValidationException(VALIDATION_FAILED, THAN_DATE_END);
        }
    }

    private void checkIsInnerGroupForCreateUpdate() {
      JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
            if (!jwtUser.isInnerGroup()) {
                throw new AccessDeniedException(A_USER_FROM_NOT_INNER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_TEMPLATE);
            }
    }

}
