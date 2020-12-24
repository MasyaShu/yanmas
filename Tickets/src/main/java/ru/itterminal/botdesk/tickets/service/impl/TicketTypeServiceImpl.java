package ru.itterminal.botdesk.tickets.service.impl;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.projection.TicketTypeUniqueFields;
import ru.itterminal.botdesk.tickets.repository.TicketTypeRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketTypeOperationValidator;

import java.util.List;

@Slf4j
@Service
@Transactional
public class TicketTypeServiceImpl extends
        CrudServiceWithAccountImpl<TicketType, TicketTypeOperationValidator, TicketTypeRepository> {


    private static final String START_FIND_TICKET_TYPES_BY_UNIQUE_FIELDS =
            "Start find ticket type by unique fields, name: {} and not id: {} and not account: {}";

    public List<TicketTypeUniqueFields> findByUniqueFields(TicketType ticketType) {
        log.trace(START_FIND_TICKET_TYPES_BY_UNIQUE_FIELDS, ticketType.getName(), ticketType.getId(), ticketType.getAccount());
        return repository.getByNameAndAccount_IdAndIdNot(ticketType.getName(), ticketType.getAccount().getId(), ticketType.getId());
    }
}
