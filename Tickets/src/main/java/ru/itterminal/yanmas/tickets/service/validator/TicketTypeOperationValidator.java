package ru.itterminal.yanmas.tickets.service.validator;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationContext;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.yanmas.security.jwt.JwtUser;
import ru.itterminal.yanmas.tickets.model.TicketType;
import ru.itterminal.yanmas.tickets.model.projection.TicketTypeUniqueFields;
import ru.itterminal.yanmas.tickets.service.impl.SettingsAccessToTicketTypesServiceImpl;
import ru.itterminal.yanmas.tickets.service.impl.TicketTypeServiceImpl;

import java.util.List;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.checkStringForEquals;

@Slf4j
@Component
@RequiredArgsConstructor
public class TicketTypeOperationValidator extends BasicOperationValidatorImpl<TicketType> {


    public static final String USER_FROM_OUTER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_TYPE =
            "A user from outer group cannot create or update ticket type";
    public static final String ACCESS_IS_DENIED_FOR_SEARCHING_BY_PASSED_TICKET_TYPE_ID =
            "Access is denied for searching by passed groupId";
    private final TicketTypeServiceImpl service;
    private final ApplicationContext appContext;


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
                throw new AccessDeniedException(USER_FROM_OUTER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_TYPE);
            }
        }
    }

    @Override
    public void checkAccessBeforeCreate(TicketType entity) {
        checkIsInnerGroupForCreateUpdate();
    }

    @Override
    public void checkAccessBeforeUpdate(TicketType entity) {
        checkIsInnerGroupForCreateUpdate();
    }

    @Override
    public void checkAccessBeforeRead(TicketType entity) {
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        var accessToTicketTypesService =
                (SettingsAccessToTicketTypesServiceImpl) appContext.getBean("settingsAccessToTicketTypesServiceImpl");
        var permittedTicketTypes = accessToTicketTypesService.getPermittedTicketTypes(jwtUser.getId());
        if (permittedTicketTypes!= null && !permittedTicketTypes.contains(entity)) {
            throw new AccessDeniedException(ACCESS_IS_DENIED_FOR_SEARCHING_BY_PASSED_TICKET_TYPE_ID);
        }
    }
}
