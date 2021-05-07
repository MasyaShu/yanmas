package ru.itterminal.yanmas.tickets.service.impl;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.yanmas.aau.service.business_handler.impl.CrudServiceWithBusinessHandlerImpl;
import ru.itterminal.yanmas.aau.service.impl.AccountServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.EntityNotExistException;
import ru.itterminal.yanmas.integration.across_modules.CompletedVerificationAccount;
import ru.itterminal.yanmas.tickets.model.TicketStatus;
import ru.itterminal.yanmas.tickets.repository.TicketStatusRepository;
import ru.itterminal.yanmas.tickets.service.business_handler.TicketStatusBusinessHandler;

import java.util.List;
import java.util.UUID;

import static java.lang.String.format;
import static ru.itterminal.yanmas.aau.service.CrudServiceWithAccount.FIND_INVALID_MESSAGE_WITH_ACCOUNT;

@Service
public class TicketStatusServiceImpl extends
        CrudServiceWithBusinessHandlerImpl<TicketStatus, TicketStatusBusinessHandler, TicketStatusRepository>
        implements CompletedVerificationAccount {


    private final AccountServiceImpl accountService;
    private final UserServiceImpl userService;

    public TicketStatusServiceImpl(List<EntityValidator<TicketStatus>> validators,
                                   AccountServiceImpl accountService,
                                   UserServiceImpl userService) {
        this.validators = validators;
        this.accountService = accountService;
        this.userService = userService;
    }

    private static final String IS_PREDEFINED_TRUE = "IsPredefinedTrue";
    public static final String CANCELED = "Canceled";
    public static final String REOPENED = "Reopened";
    public static final String FINISHED = "Finished";
    public static final String STARTED = "Started";

    @Transactional(readOnly = true)
    public TicketStatus findStartedPredefinedStatus(UUID accountId) {
        return repository.getByIsStartedPredefinedTrueAndAccount_Id(accountId).orElseThrow(
                () -> {
                    String errorMessage = format(FIND_INVALID_MESSAGE_WITH_ACCOUNT, IS_PREDEFINED_TRUE, STARTED, accountId);
                    throw new EntityNotExistException(errorMessage);
                }
        );
    }

    @Transactional(readOnly = true)
    public TicketStatus findCanceledPredefinedStatus(UUID accountId) {
        return repository.getByIsCanceledPredefinedTrueAndAccount_Id(accountId).orElseThrow(
                () -> {
                    String errorMessage = format(FIND_INVALID_MESSAGE_WITH_ACCOUNT, IS_PREDEFINED_TRUE, CANCELED, accountId);
                    throw new EntityNotExistException(errorMessage);
                }
        );
    }

    @Transactional(readOnly = true)
    public TicketStatus findReopenedPredefinedStatus(UUID accountId) {
        return repository.getByIsReopenedPredefinedTrueAndAccount_Id(accountId).orElseThrow(
                () -> {
                    String errorMessage = format(FIND_INVALID_MESSAGE_WITH_ACCOUNT, IS_PREDEFINED_TRUE, REOPENED, accountId);
                    throw new EntityNotExistException(errorMessage);
                }
        );
    }

    @Transactional(readOnly = true)
    public TicketStatus findFinishedPredefinedStatus(UUID accountId) {
        return repository.getByIsFinishedPredefinedTrueAndAccount_Id(accountId).orElseThrow(
                () -> {
                    String errorMessage = format(FIND_INVALID_MESSAGE_WITH_ACCOUNT, IS_PREDEFINED_TRUE, FINISHED, accountId);
                    throw new EntityNotExistException(errorMessage);
                }
        );
    }

    @Override
    @Transactional(noRollbackForClassName = {"EntityNotExistException"})
    public void actionAfterCompletedVerificationAccount(UUID accountId, UUID idCurrentUser) {
        var account = accountService.findById(accountId);
        var currentUser = userService.findById(idCurrentUser);
        try {
            findStartedPredefinedStatus(accountId);
        } catch (EntityNotExistException e) {
            var startedPredefinedStatus = TicketStatus.builder()
                    .account(account)
                    .isFinishedPredefined(false)
                    .isCanceledPredefined(false)
                    .isStartedPredefined(true)
                    .isReopenedPredefined(false)
                    .name(STARTED)
                    .sortIndex(100)
                    .build();
            create(startedPredefinedStatus, currentUser);
        }

        try {
            findReopenedPredefinedStatus(accountId);
        } catch (EntityNotExistException e) {
            var reopenedPredefinedStatus = TicketStatus.builder()
                    .account(account)
                    .isFinishedPredefined(false)
                    .isCanceledPredefined(false)
                    .isStartedPredefined(false)
                    .isReopenedPredefined(true)
                    .name(REOPENED)
                    .sortIndex(200)
                    .build();
            create(reopenedPredefinedStatus, currentUser);
        }

        try {
            findFinishedPredefinedStatus(accountId);
        } catch (EntityNotExistException e) {
            var finishedPredefinedStatus = TicketStatus.builder()
                    .account(account)
                    .isFinishedPredefined(true)
                    .isCanceledPredefined(false)
                    .isStartedPredefined(false)
                    .isReopenedPredefined(false)
                    .name(FINISHED)
                    .sortIndex(300)
                    .build();
            create(finishedPredefinedStatus, currentUser);
        }

        try {
            findCanceledPredefinedStatus(accountId);
        } catch (EntityNotExistException e) {
            var canceledPredefinedStatus = TicketStatus.builder()
                    .account(account)
                    .isFinishedPredefined(false)
                    .isCanceledPredefined(true)
                    .isStartedPredefined(false)
                    .isReopenedPredefined(false)
                    .name(CANCELED)
                    .sortIndex(400)
                    .build();
            create(canceledPredefinedStatus, currentUser);
        }
    }
}
