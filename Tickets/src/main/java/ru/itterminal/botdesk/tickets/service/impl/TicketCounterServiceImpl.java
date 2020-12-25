package ru.itterminal.botdesk.tickets.service.impl;

import java.util.UUID;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceImpl;
import ru.itterminal.botdesk.tickets.model.TicketCounter;
import ru.itterminal.botdesk.tickets.repository.TicketCounterRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketCounterOperationValidator;

@Slf4j
@Service
@Transactional
public class TicketCounterServiceImpl
        extends CrudServiceImpl<TicketCounter, TicketCounterOperationValidator, TicketCounterRepository> {

    public static final String START =
            "Start get number for ticket by account id: {}";
    public static final String FINISH =
            "Finish get number for ticket by account id: {}";

    public Long getTicketNumber(UUID accountId) {
        log.debug(START, accountId);
        Long ticketNumber;
        TicketCounter ticketCounter = repository.findById(accountId).orElseGet(() -> {
            var newTicketCounter = TicketCounter.builder()
                    .currentNumber(1L)
                    .build();
            return create(newTicketCounter);
        });
        ticketNumber = ticketCounter.getCurrentNumber();
        try {
            ticketCounter.setCurrentNumber(ticketNumber + 1);
            update(ticketCounter);
        }
        catch (Exception e) {
            ticketNumber = getTicketNumber(accountId);
        }
        log.debug(FINISH, accountId);
        return ticketNumber;
    }
}
