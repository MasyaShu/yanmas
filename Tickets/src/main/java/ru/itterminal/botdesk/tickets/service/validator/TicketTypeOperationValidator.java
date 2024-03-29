package ru.itterminal.botdesk.tickets.service.validator;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.projection.TicketTypeUniqueFields;
import ru.itterminal.botdesk.tickets.service.impl.TicketTypeServiceImpl;

import java.util.List;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.checkStringForEquals;

@Slf4j
@Component
@RequiredArgsConstructor
public class TicketTypeOperationValidator extends BasicOperationValidatorImpl<TicketType> {

    public static final String A_USER_FROM_NOT_INNER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_TYPE =
            "A user from not inner group cannot create or update ticket type";
    private final TicketTypeServiceImpl service;

    @Override
    public boolean beforeCreate(TicketType entity) {
        super.beforeCreate(entity);
        checkIsInnerGroupForCreateUpdate();
        return true;
    }

    @Override
    public boolean beforeUpdate(TicketType entity) {
        super.beforeUpdate(entity);
        checkIsInnerGroupForCreateUpdate();
        return true;
    }

    @Override
    public boolean checkUniqueness(TicketType entity) {
        log.trace(CHECK_UNIQUENESS, entity);
        List<TicketTypeUniqueFields> foundTicketTypes = service.findByUniqueFields(entity);
        if (!foundTicketTypes.isEmpty()) {
            String validatedField = "name";
            checkStringForEquals(entity.getName(), foundTicketTypes.get(0).getName(),
                    validatedField, format(NOT_UNIQUE_MESSAGE, validatedField));
        }
        log.trace(FIELDS_UNIQUE, entity);
        return true;
    }

    private void checkIsInnerGroupForCreateUpdate() {
        if (!SecurityContextHolder.getContext().getAuthentication().getName().contains("anonymous")) {
            JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
            if (!jwtUser.isInnerGroup()) {
                throw new AccessDeniedException(A_USER_FROM_NOT_INNER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_TYPE);
            }
        }
    }
}
