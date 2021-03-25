package ru.itterminal.botdesk.tickets.service.validator;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createLogicalValidationException;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.tickets.model.GroupTicketTypes;
import ru.itterminal.botdesk.tickets.service.impl.GroupTicketTypesServiceImpl;

@Slf4j
@Component
@RequiredArgsConstructor
public class GroupTicketTypesOperationValidator extends BasicOperationValidatorImpl<GroupTicketTypes> {

    private final GroupTicketTypesServiceImpl service;
    private static final String NAME = "name";
    public static final String USER_FROM_OUTER_GROUP_CANNOT_CREATE_OR_UPDATE_GROUP_OF_TICKET_TYPES =
            "A user from outer group cannot create or update group of ticket types";

    @Override
    public boolean checkUniqueness(GroupTicketTypes entity) {
        log.trace(CHECK_UNIQUENESS, entity);
        var groupTicketTypesList = service.findByUniqueFields(entity);
        if (!groupTicketTypesList.isEmpty()) {
            throw createLogicalValidationException(NOT_UNIQUE_CODE, format(NOT_UNIQUE_MESSAGE, NAME));
        }
        log.trace(FIELDS_UNIQUE, entity);
        return true;
    }

    @Override
    public void checkAccessBeforeCreate(GroupTicketTypes entity) {
        checkIsInnerGroupForCreateUpdate();
    }

    @Override
    public void checkAccessBeforeUpdate(GroupTicketTypes entity) {
        checkIsInnerGroupForCreateUpdate();
    }

    private void checkIsInnerGroupForCreateUpdate() {
        if (!SecurityContextHolder.getContext().getAuthentication().getName().contains("anonymous")) {
            JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
            if (!jwtUser.isInnerGroup()) {
                throw new AccessDeniedException(USER_FROM_OUTER_GROUP_CANNOT_CREATE_OR_UPDATE_GROUP_OF_TICKET_TYPES);
            }
        }
    }
}
