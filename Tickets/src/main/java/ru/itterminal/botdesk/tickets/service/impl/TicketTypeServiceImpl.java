package ru.itterminal.botdesk.tickets.service.impl;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekObjectForNull;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekStringForNullOrEmpty;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.projection.TicketTypeUniqueFields;
import ru.itterminal.botdesk.tickets.repository.TicketTypeRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketTypeOperationValidator;

@Slf4j
@Service
@Transactional
public class TicketTypeServiceImpl extends
        CrudServiceWithAccountImpl<TicketType, TicketTypeOperationValidator, TicketTypeRepository> {

    private static final String TICKET_TYPE_IS_NULL =
            "Not found ticket type by unique fields, ticketType is null";
    private static final String ACCOUNT_IS_NULL =
            "Not found ticket type by unique fields, account is null";
    private static final String NAME_IS_NULL =
            "Not found ticket type by unique fields, name is null";
    private static final String UNIQUE_FIELDS_ID_IS_NULL =
            "Not found ticket type by unique fields, id is null";
    private static final String START_FIND_TICKET_TYPES_BY_UNIQUE_FIELDS =
            "Start find ticket type by unique fields, name: {} and not id: {} and not account: {}";
    private static final String NAME_IS_EMPTY = "Name is empty";
    private static final String CANT_FIND_BY_UNIQUE = "Can't find by unique fields";

    @SuppressWarnings("DuplicatedCode")
    public List<TicketTypeUniqueFields> findByUniqueFields(TicketType ticketType) {
        chekObjectForNull(ticketType, TICKET_TYPE_IS_NULL, EntityNotExistException.class);
        chekStringForNullOrEmpty(ticketType.getName(), NAME_IS_NULL,
                NAME_IS_EMPTY, EntityNotExistException.class, CANT_FIND_BY_UNIQUE);
        chekObjectForNull(ticketType.getId(), UNIQUE_FIELDS_ID_IS_NULL, EntityNotExistException.class);
        chekObjectForNull(ticketType.getAccount(), ACCOUNT_IS_NULL,
                EntityNotExistException.class);
        log.trace(START_FIND_TICKET_TYPES_BY_UNIQUE_FIELDS, ticketType.getName(), ticketType.getId(), ticketType.getAccount());
        return repository.getByNameAndAccount_IdAndIdNot(ticketType.getName(), ticketType.getAccount().getId(), ticketType
                .getId());
    }

    @Override
    public TicketType update(TicketType entity) {
        TicketType entityFromDatabase = findByIdAndAccountId(entity.getId(), entity.getAccount().getId());
        entity.setIsPredefined(entityFromDatabase.getIsPredefined());
        return super.update(entity);
    }
}
