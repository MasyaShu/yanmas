package ru.itterminal.yanmas.tickets.service.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.yanmas.aau.service.business_handler.impl.CrudServiceWithBusinessHandlerImpl;
import ru.itterminal.yanmas.aau.service.impl.AccountServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.commons.exception.EntityNotExistException;
import ru.itterminal.yanmas.integration.across_modules.CompletedVerificationAccount;
import ru.itterminal.yanmas.tickets.model.TicketType;
import ru.itterminal.yanmas.tickets.repository.TicketTypeRepository;
import ru.itterminal.yanmas.tickets.service.business_handler.TicketTypeBusinessHandler;

import java.util.UUID;

import static java.lang.String.format;
import static ru.itterminal.yanmas.aau.service.CrudServiceWithAccount.FIND_INVALID_MESSAGE_WITH_ACCOUNT;

@Service
@AllArgsConstructor
public class TicketTypeServiceImpl extends CrudServiceWithBusinessHandlerImpl
        <TicketType, TicketTypeBusinessHandler, TicketTypeRepository>
        implements CompletedVerificationAccount {

    public static final String DEFAULT_TYPE = "Default type";
    private static final String PREDEFINED_TICKET_TYPE_FOR_NEW_TICKET = "Predefined ticket type for new ticket";

    private final AccountServiceImpl accountService;
    private final UserServiceImpl userService;

    @Transactional(readOnly = true)
    public TicketType findStartedPredefinedTicketTypeForNewTicket(UUID accountId) {
        return repository.getByIsPredefinedForNewTicketTrueAndAccount_Id(accountId).orElseThrow(
                () -> {
                    String errorMessage = format(FIND_INVALID_MESSAGE_WITH_ACCOUNT, PREDEFINED_TICKET_TYPE_FOR_NEW_TICKET, "true", accountId);
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
            findStartedPredefinedTicketTypeForNewTicket(accountId);
        } catch (EntityNotExistException e) {
            var predefinedTicketType = TicketType.builder()
                    .isPredefinedForNewTicket(true)
                    .account(account)
                    .name(DEFAULT_TYPE)
                    .build();
            create(predefinedTicketType, currentUser);
        }
    }
}
