package ru.itterminal.botdesk.tickets.service.impl;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.projection.TicketStatusUniqueFields;
import ru.itterminal.botdesk.tickets.repository.TicketStatusRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketStatusOperationValidator;

import java.util.List;
import java.util.UUID;

import static java.lang.String.format;

@Slf4j
@Service
@Transactional
@AllArgsConstructor
public class TicketStatusServiceImpl extends
        CrudServiceWithAccountImpl<TicketStatus, TicketStatusOperationValidator, TicketStatusRepository> {

    private final AccountServiceImpl accountService;

    private static final String START_FIND_TICKET_TYPES_BY_UNIQUE_FIELDS =
            "Start find ticket status by unique fields, name: {} and not id: {} and not account: {}";
    private static final String START_FIND_STATUS_FOR_ACCOUNT = "Start find {} predefined status for account: {}";
    private static final String IS_PREDEFINED_TRUE = "IsPredefinedTrue";
    private static final String START = "'Start'";
    private static final String CANCELED = "'Canceled'";
    private static final String REOPENED = "'Reopened'";
    private static final String FINISHED = "'Finished'";

    @Transactional(readOnly = true)
    public List<TicketStatusUniqueFields> findByUniqueFields(TicketStatus ticketStatus) {
        log.trace(START_FIND_TICKET_TYPES_BY_UNIQUE_FIELDS, ticketStatus.getName(), ticketStatus.getId(), ticketStatus.getAccount());
        return repository.getByNameAndAccount_IdAndIdNot(ticketStatus.getName(), ticketStatus.getAccount().getId(), ticketStatus
                .getId());
    }

    @Transactional(readOnly = true)
    public TicketStatus findStartedPredefinedStatus(UUID accountId) {
        log.trace(START_FIND_STATUS_FOR_ACCOUNT, START, accountId);
        return repository.getByIsStartedPredefinedTrueAndAccount_Id(accountId).orElseThrow(
                () -> {
                    String errorMessage = format(FIND_INVALID_MESSAGE_WITH_ACCOUNT, IS_PREDEFINED_TRUE, START, accountId);
                    log.error(errorMessage);
                    throw new EntityNotExistException(errorMessage);
                }
        );
    }

    @Transactional(readOnly = true)
    public TicketStatus findCanceledPredefinedStatus(UUID accountId) {
        log.trace(START_FIND_STATUS_FOR_ACCOUNT, CANCELED, accountId);
        return repository.getByIsCanceledPredefinedTrueAndAccount_Id(accountId).orElseThrow(
                () -> {
                    String errorMessage = format(FIND_INVALID_MESSAGE_WITH_ACCOUNT, IS_PREDEFINED_TRUE, CANCELED, accountId);
                    log.error(errorMessage);
                    throw new EntityNotExistException(errorMessage);
                }
        );
    }

    @Transactional(readOnly = true)
    public TicketStatus findReopenedPredefinedStatus(UUID accountId) {
        log.trace(START_FIND_STATUS_FOR_ACCOUNT, REOPENED, accountId);
        return repository.getByIsReopenedPredefinedTrueAndAccount_Id(accountId).orElseThrow(
                () -> {
                    String errorMessage = format(FIND_INVALID_MESSAGE_WITH_ACCOUNT, IS_PREDEFINED_TRUE, REOPENED, accountId);
                    log.error(errorMessage);
                    throw new EntityNotExistException(errorMessage);
                }
        );
    }

    @Transactional(readOnly = true)
    public TicketStatus findFinishedPredefinedStatus(UUID accountId) {
        log.trace(START_FIND_STATUS_FOR_ACCOUNT, FINISHED, accountId);
        return repository.getByIsFinishedPredefinedTrueAndAccount_Id(accountId).orElseThrow(
                () -> {
                    String errorMessage = format(FIND_INVALID_MESSAGE_WITH_ACCOUNT, IS_PREDEFINED_TRUE, FINISHED, accountId);
                    log.error(errorMessage);
                    throw new EntityNotExistException(errorMessage);
                }
        );
    }

    public void createPredefinedStatus(UUID accountId) {
        var startedPredefinedStatus = TicketStatus.builder()
                .account(accountService.findById(accountId))
                .isFinishedPredefined(true)
                .name("Started")
                .id(UUID.randomUUID())
                .sortIndex(100)
                .build();
        create(startedPredefinedStatus);

        var reopenedPredefinedStatus = TicketStatus.builder()
                .account(accountService.findById(accountId))
                .isFinishedPredefined(true)
                .name("Reopened")
                .id(UUID.randomUUID())
                .sortIndex(200)
                .build();
        create(reopenedPredefinedStatus);

        var finishedPredefinedStatus = TicketStatus.builder()
                .account(accountService.findById(accountId))
                .isFinishedPredefined(true)
                .name("Finished")
                .id(UUID.randomUUID())
                .sortIndex(300)
                .build();
        create(finishedPredefinedStatus);


        var canceledPredefinedStatus = TicketStatus.builder()
                .account(accountService.findById(accountId))
                .isFinishedPredefined(true)
                .name("Canceled")
                .id(UUID.randomUUID())
                .sortIndex(400)
                .build();
        create(canceledPredefinedStatus);
    }
}
