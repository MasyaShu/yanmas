package ru.itterminal.botdesk.tickets.service.impl;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.util.CommonConstants.NOT_FOUND_ENTITY_BY_ID_AND_ACCOUNT_ID;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekObjectForNull;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekStringForNullOrEmpty;

import java.util.List;
import java.util.UUID;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.projection.TicketStatusUniqueFields;
import ru.itterminal.botdesk.tickets.repository.TicketStatusRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketStatusOperationValidator;

@Slf4j
@Service
@Transactional
public class TicketStatusServiceImpl extends
        CrudServiceWithAccountImpl<TicketStatus, TicketStatusOperationValidator, TicketStatusRepository> {

    private static final String TICKET_TYPE_IS_NULL =
            "Not found ticket status by unique fields, ticketStatus is null";
    private static final String ACCOUNT_IS_NULL =
            "Not found ticket status by unique fields, account is null";
    private static final String NAME_IS_NULL =
            "Not found ticket status by unique fields, name is null";
    private static final String UNIQUE_FIELDS_ID_IS_NULL =
            "Not found ticket status by unique fields, id is null";
    private static final String NAME_IS_EMPTY = "Name is empty";
    private static final String START_FIND_TICKET_TYPES_BY_UNIQUE_FIELDS =
            "Start find ticket status by unique fields, name: {} and not id: {} and not account: {}";
    private static final String CANT_FIND_BY_UNIQUE = "Can't find by unique fields";

    @SuppressWarnings("DuplicatedCode")
    public List<TicketStatusUniqueFields> findByUniqueFields(TicketStatus ticketStatus) {
        chekObjectForNull(ticketStatus, TICKET_TYPE_IS_NULL, EntityNotExistException.class);
        chekStringForNullOrEmpty(ticketStatus.getName(), NAME_IS_NULL,
                NAME_IS_EMPTY, EntityNotExistException.class, CANT_FIND_BY_UNIQUE);
        chekObjectForNull(ticketStatus.getId(), UNIQUE_FIELDS_ID_IS_NULL, EntityNotExistException.class);
        chekObjectForNull(ticketStatus.getAccount(), ACCOUNT_IS_NULL,
                EntityNotExistException.class);
        log.trace(START_FIND_TICKET_TYPES_BY_UNIQUE_FIELDS, ticketStatus.getName(), ticketStatus.getId(), ticketStatus.getAccount());
        return repository.getByNameAndAccount_IdAndIdNot(ticketStatus.getName(), ticketStatus.getAccount().getId(), ticketStatus
                .getId());
    }

    @Override
    public TicketStatus update(TicketStatus entity) {
        TicketStatus entityFromDatabase = super.findById(entity.getId());
        entity.setIsCanceledPredefined(entityFromDatabase.getIsCanceledPredefined());
        entity.setIsFinishedPredefined(entityFromDatabase.getIsFinishedPredefined());
        entity.setIsReopenedPredefined(entityFromDatabase.getIsReopenedPredefined());
        entity.setIsStartedPredefined(entityFromDatabase.getIsStartedPredefined());
        return super.update(entity);
    }
}
