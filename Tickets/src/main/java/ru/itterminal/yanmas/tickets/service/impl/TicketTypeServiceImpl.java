package ru.itterminal.yanmas.tickets.service.impl;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.yanmas.aau.service.impl.AccountServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.yanmas.commons.exception.EntityNotExistException;
import ru.itterminal.yanmas.integration.across_modules.CompletedVerificationAccount;
import ru.itterminal.yanmas.tickets.model.TicketType;
import ru.itterminal.yanmas.tickets.repository.TicketTypeRepository;
import ru.itterminal.yanmas.tickets.service.validator.TicketTypeOperationValidator;

import java.util.UUID;

import static java.lang.String.format;

@Slf4j
@Service
@AllArgsConstructor
public class TicketTypeServiceImpl extends
        CrudServiceWithAccountImpl<TicketType, TicketTypeOperationValidator, TicketTypeRepository> implements CompletedVerificationAccount {

    public static final String DEFAULT_TYPE = "Default type";
    private final AccountServiceImpl accountService;

    private static final String START_FIND_PREDEFINED_TYPE_FOR_NEW_TICKET_STATUS_FOR_ACCOUNT = "Start find predefined type for new ticket status for account: {}";
    private static final String PREDEFINED_TICKET_TYPE_FOR_NEW_TICKET = "Predefined ticket type for new ticket";

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
    @Transactional(noRollbackForClassName = {"EntityNotExistException"})
    public void actionAfterCompletedVerificationAccount(UUID accountId, UUID idCurrentUser) {
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

    @Override
    protected void setNestedObjectsOfEntityBeforeCreate(TicketType entity) {
        setNestedObjectsOfEntity(entity);
        entity.setDeleted(false);
    }

    @Override
    protected void setNestedObjectsOfEntityBeforeUpdate(TicketType entity) {
        setNestedObjectsOfEntity(entity);
    }

    private void setNestedObjectsOfEntity(TicketType entity) {
        entity.setAccount(accountService.findById(entity.getAccount().getId()));
    }
}
