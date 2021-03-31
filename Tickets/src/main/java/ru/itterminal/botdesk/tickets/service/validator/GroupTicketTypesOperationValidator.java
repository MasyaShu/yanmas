package ru.itterminal.botdesk.tickets.service.validator;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createLogicalValidationException;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.security.jwt.JwtUserBuilder;
import ru.itterminal.botdesk.tickets.model.GroupTicketTypes;
import ru.itterminal.botdesk.tickets.service.impl.GroupTicketTypesServiceImpl;

@Slf4j
@Component
@RequiredArgsConstructor
public class GroupTicketTypesOperationValidator extends BasicOperationValidatorImpl<GroupTicketTypes> {

    private final GroupTicketTypesServiceImpl service;
    private final JwtUserBuilder jwtUserBuilder;

    private static final String NAME = "name";

    @Override
    public boolean checkUniqueness(GroupTicketTypes entity) {
        log.trace(CHECK_UNIQUENESS, entity);
        var groupTicketTypesList = service.findByUniqueFields(entity);
        if (!groupTicketTypesList.isEmpty()) {
            log.error(format(NOT_UNIQUE_MESSAGE, NAME));
            throw createLogicalValidationException(NOT_UNIQUE_CODE, format(NOT_UNIQUE_MESSAGE, NAME));
        }
        log.trace(FIELDS_UNIQUE, entity);
        return true;
    }

    @Override
    public void checkAccessBeforeCreate(GroupTicketTypes entity) {
        jwtUserBuilder.throwAccessDeniedExceptionIfCurrentUserFromOuterGroup();
    }

    @Override
    public void checkAccessBeforeUpdate(GroupTicketTypes entity) {
        jwtUserBuilder.throwAccessDeniedExceptionIfCurrentUserFromOuterGroup();
    }
}
