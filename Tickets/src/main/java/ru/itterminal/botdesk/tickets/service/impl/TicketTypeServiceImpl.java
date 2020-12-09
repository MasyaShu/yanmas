package ru.itterminal.botdesk.tickets.service.impl;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceImpl;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.projection.TicketTypesUniqueFields;
import ru.itterminal.botdesk.tickets.repository.TicketTypeRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketTypeOperationValidator;

import java.util.List;
import java.util.UUID;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.util.CommonConstants.NOT_FOUND_ENTITY_BY_ID_AND_ACCOUNT_ID;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekObjectForNull;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekStringForNullOrEmpty;

@Slf4j
@Service
@Transactional
public class TicketTypeServiceImpl extends CrudServiceImpl<TicketType, TicketTypeOperationValidator, TicketTypeRepository> {

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
    private static final String START_FIND_TICKET_TYPES_BY_ID_AND_ACCOUNT_ID = "Start find ticket type by id: {} and accountId: {}";
    public static final String ENTITY_TICKET_TYPES_NAME = TicketType.class.getSimpleName();
    private static final String NAME_IS_EMPTY = "Name is empty";
    private static final String CANT_FIND_BY_UNIQUE = "Can't find by unique fields";

    @SuppressWarnings("DuplicatedCode")
    public List<TicketTypesUniqueFields> findByUniqueFields(TicketType ticketType) {
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
        TicketType entityFromDatabase = super.findById(entity.getId());
        entity.setIsPredefined(entityFromDatabase.getIsPredefined());
        if (entity.getComment() == null) {
            entity.setComment(entityFromDatabase.getComment());
        }
        return super.update(entity);
    }

    @SuppressWarnings("DuplicatedCode")
    @Transactional(readOnly = true)
    public TicketType findByIdAndAccountId(UUID id, UUID accountId) {
        chekObjectForNull(id, format(NOT_FOUND_ENTITY_BY_ID_AND_ACCOUNT_ID, ENTITY_TICKET_TYPES_NAME, id, accountId),
                EntityNotExistException.class);
        chekObjectForNull(accountId, format(NOT_FOUND_ENTITY_BY_ID_AND_ACCOUNT_ID, ENTITY_TICKET_TYPES_NAME, id, accountId),
                EntityNotExistException.class);
        log.trace(START_FIND_TICKET_TYPES_BY_ID_AND_ACCOUNT_ID, id, accountId);
        return repository.getByIdAndAccount_Id(id, accountId).orElseThrow(
                () -> new EntityNotExistException(format(NOT_FOUND_ENTITY_BY_ID_AND_ACCOUNT_ID, ENTITY_TICKET_TYPES_NAME, id,
                        accountId))
        );
    }
}
