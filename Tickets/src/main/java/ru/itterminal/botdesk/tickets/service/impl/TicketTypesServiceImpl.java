package ru.itterminal.botdesk.tickets.service.impl;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceImpl;
import ru.itterminal.botdesk.tickets.model.TicketTypes;
import ru.itterminal.botdesk.tickets.model.projection.TicketTypesUniqueFields;
import ru.itterminal.botdesk.tickets.repository.TicketTypesRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketTypesOperationValidator;

import java.util.List;

import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekObjectForNull;


@Slf4j
@Service
@Transactional
public class TicketTypesServiceImpl extends CrudServiceImpl<TicketTypes, TicketTypesOperationValidator, TicketTypesRepository> {

    private static final String NOT_FOUND_TICKET_TYPES_BY_UNIQUE_FIELDS_GROUP_IS_NULL =
            "Not found ticket type by unique fields, group is null";
    private static final String NOT_FOUND_TICKET_TYPES_BY_UNIQUE_FIELDS_ACCOUNT_IS_NULL =
            "Not found ticket type by unique fields, account is null";
    private static final String NOT_FOUND_TICKET_TYPES_BY_UNIQUE_FIELDS_NAME_IS_NULL =
            "Not found ticket type by unique fields, name is null";
    private static final String NOT_FOUND_TICKET_TYPES_BY_UNIQUE_FIELDS_ID_IS_NULL =
            "Not found ticket type by unique fields, id is null";
    private static final String START_FIND_TICKET_TYPES_BY_UNIQUE_FIELDS =
            "Start find ticket type by unique fields, name: {} and not id: {} and not account: {}";



    @SuppressWarnings("DuplicatedCode")
    public List<TicketTypesUniqueFields> findByUniqueFields(TicketTypes ticketTypes) {
        chekObjectForNull(ticketTypes, NOT_FOUND_TICKET_TYPES_BY_UNIQUE_FIELDS_GROUP_IS_NULL, EntityNotExistException.class);
        chekObjectForNull(ticketTypes.getName(), NOT_FOUND_TICKET_TYPES_BY_UNIQUE_FIELDS_NAME_IS_NULL,
                EntityNotExistException.class);
        chekObjectForNull(ticketTypes.getId(), NOT_FOUND_TICKET_TYPES_BY_UNIQUE_FIELDS_ID_IS_NULL, EntityNotExistException.class);
        chekObjectForNull(ticketTypes.getAccount(), NOT_FOUND_TICKET_TYPES_BY_UNIQUE_FIELDS_ACCOUNT_IS_NULL,
                EntityNotExistException.class);
        log.trace(START_FIND_TICKET_TYPES_BY_UNIQUE_FIELDS, ticketTypes.getName(), ticketTypes.getId(), ticketTypes.getAccount());
        return repository.getByNameAndAccount_IdAndIdNot(ticketTypes.getName(), ticketTypes.getAccount().getId(), ticketTypes.getId());
    }


}
