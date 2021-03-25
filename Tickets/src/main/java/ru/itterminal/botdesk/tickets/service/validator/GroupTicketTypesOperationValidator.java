package ru.itterminal.botdesk.tickets.service.validator;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createLogicalValidationException;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.tickets.model.GroupTicketTypes;
import ru.itterminal.botdesk.tickets.service.impl.GroupTicketTypesServiceImpl;

@Slf4j
@Component
@RequiredArgsConstructor
public class GroupTicketTypesOperationValidator extends BasicOperationValidatorImpl<GroupTicketTypes> {

    private static final String NAME = "name";
    private final GroupTicketTypesServiceImpl service;

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
}
