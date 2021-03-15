package ru.itterminal.botdesk.tickets.service.impl;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.model.EntityConverter;
import ru.itterminal.botdesk.integration.across_modules.CompletedVerificationAccount;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.dto.TicketStatusDto;
import ru.itterminal.botdesk.tickets.model.projection.TicketStatusUniqueFields;
import ru.itterminal.botdesk.tickets.repository.TicketStatusRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketStatusOperationValidator;

import java.util.List;
import java.util.UUID;

import static java.lang.String.format;

@Slf4j
@Service
@AllArgsConstructor
public class TicketStatusServiceImpl extends
        CrudServiceWithAccountImpl<TicketStatus, TicketStatusOperationValidator, TicketStatusRepository>
        implements CompletedVerificationAccount, EntityConverter<TicketStatus, TicketStatusDto> {


    private final AccountServiceImpl accountService;

    private static final String START_FIND_TICKET_TYPES_BY_UNIQUE_FIELDS =
            "Start find ticket status by unique fields, name: {} and not id: {} and not account: {}";
    private static final String START_FIND_STATUS_FOR_ACCOUNT = "Start find {} predefined status for account: {}";
    private static final String IS_PREDEFINED_TRUE = "IsPredefinedTrue";
    public static final String CANCELED = "Canceled";
    public static final String REOPENED = "Reopened";
    public static final String FINISHED = "Finished";
    public static final String STARTED = "Started";

    @Transactional(readOnly = true)
    public List<TicketStatusUniqueFields> findByUniqueFields(TicketStatus ticketStatus) {
        log.trace(START_FIND_TICKET_TYPES_BY_UNIQUE_FIELDS, ticketStatus.getName(), ticketStatus.getId(), ticketStatus.getAccount());
        return repository.getByNameAndAccount_IdAndIdNot(ticketStatus.getName(), ticketStatus.getAccount().getId(), ticketStatus
                .getId());
    }

    @Transactional(readOnly = true)
    public TicketStatus findStartedPredefinedStatus(UUID accountId) {
        log.trace(START_FIND_STATUS_FOR_ACCOUNT, STARTED, accountId);
        return repository.getByIsStartedPredefinedTrueAndAccount_Id(accountId).orElseThrow(
                () -> {
                    String errorMessage = format(FIND_INVALID_MESSAGE_WITH_ACCOUNT, IS_PREDEFINED_TRUE, STARTED, accountId);
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

    @Override
    @Transactional(noRollbackForClassName = {"EntityNotExistException"})
    public void actionAfterCompletedVerificationAccount(UUID accountId) {
        try {
            findStartedPredefinedStatus(accountId);
        } catch (EntityNotExistException e) {
            var startedPredefinedStatus = TicketStatus.builder()
                    .account(accountService.findById(accountId))
                    .isFinishedPredefined(false)
                    .isCanceledPredefined(false)
                    .isStartedPredefined(true)
                    .isReopenedPredefined(false)
                    .name(STARTED)
                    .sortIndex(100)
                    .build();
            create(startedPredefinedStatus);
        }

        try {
            findReopenedPredefinedStatus(accountId);
        } catch (EntityNotExistException e) {
            var reopenedPredefinedStatus = TicketStatus.builder()
                    .account(accountService.findById(accountId))
                    .isFinishedPredefined(false)
                    .isCanceledPredefined(false)
                    .isStartedPredefined(false)
                    .isReopenedPredefined(true)
                    .name(REOPENED)
                    .sortIndex(200)
                    .build();
            create(reopenedPredefinedStatus);
        }

        try {
            findFinishedPredefinedStatus(accountId);
        } catch (EntityNotExistException e) {
            var finishedPredefinedStatus = TicketStatus.builder()
                    .account(accountService.findById(accountId))
                    .isFinishedPredefined(true)
                    .isCanceledPredefined(false)
                    .isStartedPredefined(false)
                    .isReopenedPredefined(false)
                    .name(FINISHED)
                    .sortIndex(300)
                    .build();
            create(finishedPredefinedStatus);
        }

        try {
            findCanceledPredefinedStatus(accountId);
        } catch (EntityNotExistException e) {
            var canceledPredefinedStatus = TicketStatus.builder()
                    .account(accountService.findById(accountId))
                    .isFinishedPredefined(false)
                    .isCanceledPredefined(true)
                    .isStartedPredefined(false)
                    .isReopenedPredefined(false)
                    .name(CANCELED)
                    .sortIndex(400)
                    .build();
            create(canceledPredefinedStatus);
        }
    }

    @Override
    public TicketStatus convertRequestDtoIntoEntityWithNestedObjectsWithOnlyId(TicketStatusDto request, UUID accountId) {
        TicketStatus ticketStatus = modelMapper.map(request, TicketStatus.class);
        ticketStatus.setAccount(Account.builder().id(accountId).build());
        return ticketStatus;
    }

    @Override
    protected void setNestedObjectsOfEntityBeforeCreate(TicketStatus entity) {
        super.setNestedObjectsOfEntityBeforeCreate(entity);
        setNestedObjectsOfEntity(entity);
        entity.setIsCanceledPredefined(false);
        entity.setIsStartedPredefined(false);
        entity.setIsFinishedPredefined(false);
        entity.setIsReopenedPredefined(false);
        entity.setDeleted(false);
    }

    @Override
    protected void setNestedObjectsOfEntityBeforeUpdate(TicketStatus entity) {
        super.setNestedObjectsOfEntityBeforeUpdate(entity);
        setNestedObjectsOfEntity(entity);
    }

    private void setNestedObjectsOfEntity(TicketStatus entity) {
        entity.setAccount(accountService.findById(entity.getAccount().getId()));
    }
}
