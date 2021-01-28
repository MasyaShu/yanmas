package ru.itterminal.botdesk.tickets.service.impl;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.integration.innerflow.CompletedVerificationAccount;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.projection.TicketTypeUniqueFields;
import ru.itterminal.botdesk.tickets.repository.TicketTypeRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketTypeOperationValidator;

import java.util.List;
import java.util.UUID;

import static java.lang.String.format;

@Slf4j
@Service
@Transactional
@AllArgsConstructor
public class TicketTypeServiceImpl extends
        CrudServiceWithAccountImpl<TicketType, TicketTypeOperationValidator, TicketTypeRepository> implements CompletedVerificationAccount {

    private static final String DEFAULT_TYPE = "Default type";
    private final AccountServiceImpl accountService;

    private static final String START_FIND_TICKET_TYPES_BY_UNIQUE_FIELDS =
            "Start find ticket type by unique fields, name: {} and not id: {} and not account: {}";
    private static final String START_FIND_PREDEFINED_TYPE_FOR_NEW_TICKET_STATUS_FOR_ACCOUNT = "Start find predefined type for new ticket status for account: {}";
    private static final String PREDEFINED_TICKET_TYPE_FOR_NEW_TICKET = "Predefined ticket type for new ticket";

    @Transactional(readOnly = true)
    public List<TicketTypeUniqueFields> findByUniqueFields(TicketType ticketType) {
        log.trace(START_FIND_TICKET_TYPES_BY_UNIQUE_FIELDS, ticketType.getName(), ticketType.getId(), ticketType.getAccount());
        return repository.getByNameAndAccount_IdAndIdNot(ticketType.getName(), ticketType.getAccount().getId(), ticketType.getId());
    }

    @Transactional(readOnly = true)
    public TicketType findStartedPredefinedTicketTypeForNewTicket(UUID accountId) {
        log.trace(START_FIND_PREDEFINED_TYPE_FOR_NEW_TICKET_STATUS_FOR_ACCOUNT, accountId);
        return repository.getByIsPredefinedForNewTicketTrueAndAccount_Id(accountId).orElseThrow(
                () -> {
                    String errorMessage = format(FIND_INVALID_MESSAGE_WITH_ACCOUNT, PREDEFINED_TICKET_TYPE_FOR_NEW_TICKET, "true", accountId);
                    log.error(errorMessage);
                    throw new EntityNotExistException(errorMessage);
                }
        );
    }

    @Override
    public void createPredefinedEntity(UUID accountId) {
        try {
            findStartedPredefinedTicketTypeForNewTicket(accountId);
        } catch (EntityNotExistException e) {
            var predefinedTicketType = TicketType.builder()
                    .isPredefinedForNewTicket(true)
                    .account(accountService.findById(accountId))
                    .name(DEFAULT_TYPE)
                    .build();
            create(predefinedTicketType);
        }
    }
}
