package ru.itterminal.yanmas.tickets.service.impl;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.yanmas.aau.service.impl.AccountServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.yanmas.commons.exception.EntityNotExistException;
import ru.itterminal.yanmas.integration.across_modules.CompletedVerificationAccount;
import ru.itterminal.yanmas.tickets.model.TicketStatus;
import ru.itterminal.yanmas.tickets.repository.TicketStatusRepository;
import ru.itterminal.yanmas.tickets.service.validator.TicketStatusOperationValidator;

import java.util.UUID;

import static java.lang.String.format;

@Slf4j
@Service
@AllArgsConstructor
public class TicketStatusServiceImpl extends
        CrudServiceWithAccountImpl<TicketStatus, TicketStatusOperationValidator, TicketStatusRepository>
        implements CompletedVerificationAccount {


    private final AccountServiceImpl accountService;

    private static final String START_FIND_STATUS_FOR_ACCOUNT = "Start find {} predefined status for account: {}";
    private static final String IS_PREDEFINED_TRUE = "IsPredefinedTrue";
    public static final String CANCELED = "Canceled";
    public static final String REOPENED = "Reopened";
    public static final String FINISHED = "Finished";
    public static final String STARTED = "Started";

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
    protected void setNestedObjectsOfEntityBeforeCreate(TicketStatus entity) {
        setNestedObjectsOfEntity(entity);
        entity.setDeleted(false);
    }

    @Override
    protected void setNestedObjectsOfEntityBeforeUpdate(TicketStatus entity) {
        setNestedObjectsOfEntity(entity);
    }

    private void setNestedObjectsOfEntity(TicketStatus entity) {
        entity.setAccount(accountService.findById(entity.getAccount().getId()));
    }
}
