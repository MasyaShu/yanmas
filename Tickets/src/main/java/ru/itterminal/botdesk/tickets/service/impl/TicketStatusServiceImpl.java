package ru.itterminal.botdesk.tickets.service.impl;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.projection.TicketStatusUniqueFields;
import ru.itterminal.botdesk.tickets.repository.TicketStatusRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketStatusOperationValidator;

import java.util.List;

@Slf4j
@Service
@Transactional
public class TicketStatusServiceImpl extends
        CrudServiceWithAccountImpl<TicketStatus, TicketStatusOperationValidator, TicketStatusRepository> {

    private static final String START_FIND_TICKET_TYPES_BY_UNIQUE_FIELDS =
            "Start find ticket status by unique fields, name: {} and not id: {} and not account: {}";

    @Transactional(readOnly = true)
    public List<TicketStatusUniqueFields> findByUniqueFields(TicketStatus ticketStatus) {
        log.trace(START_FIND_TICKET_TYPES_BY_UNIQUE_FIELDS, ticketStatus.getName(), ticketStatus.getId(), ticketStatus.getAccount());
        return repository.getByNameAndAccount_IdAndIdNot(ticketStatus.getName(), ticketStatus.getAccount().getId(), ticketStatus
                .getId());
    }
}
