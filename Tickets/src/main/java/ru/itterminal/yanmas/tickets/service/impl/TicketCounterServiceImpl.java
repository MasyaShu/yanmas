package ru.itterminal.yanmas.tickets.service.impl;

import static java.lang.String.format;

import java.util.UUID;

import javax.persistence.OptimisticLockException;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.yanmas.commons.service.impl.CrudServiceImpl;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;
import ru.itterminal.yanmas.tickets.model.TicketCounter;
import ru.itterminal.yanmas.tickets.repository.TicketCounterRepository;
import ru.itterminal.yanmas.tickets.service.validator.TicketCounterOperationValidator;

@Slf4j
@Service
@Transactional
@RequiredArgsConstructor
public class TicketCounterServiceImpl
        extends CrudServiceImpl<TicketCounter, TicketCounterOperationValidator, TicketCounterRepository> {

    public static final String START_NEXT =
            "Start get next number for ticket by account id: {}";
    public static final String FINISH_NEXT =
            "Finish get next number for ticket by account id: {}";
    public static final String START_CURRENT =
            "Start get current number for ticket by account id: {}";
    public static final String FINISH_CURRENT =
            "Finish get current number for ticket by account id: {}";

    private final JwtUserBuilder jwtUserBuilder;

    @Override
    @Transactional
    public TicketCounter create(TicketCounter entity) {
        validator.checkAccessBeforeCreate(entity);
        validator.logicalValidationBeforeCreate(entity);
        log.trace(format(CREATE_INIT_MESSAGE, entity.getClass().getSimpleName(), entity.toString()));
        entity.setId(jwtUserBuilder.getJwtUser().getAccountId());
        entity.generateDisplayName();
        validator.checkUniqueness(entity);
        var createdEntity = repository.create(entity);
        log.trace(format(CREATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), createdEntity.toString()));
        return createdEntity;
    }

    public Long getNextTicketNumber(UUID accountId) {
        log.debug(START_NEXT, accountId);
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
        catch (OptimisticLockException e) {
            ticketNumber = getNextTicketNumber(accountId);
        }
        log.debug(FINISH_NEXT, accountId);
        return ticketNumber;
    }

    public TicketCounter getTicketCounter(UUID accountId) {
        log.debug(START_CURRENT, accountId);
        TicketCounter ticketCounter = repository.findById(accountId).orElseGet(() -> {
            var newTicketCounter = TicketCounter.builder()
                    .currentNumber(0L)
                    .build();
            return create(newTicketCounter);
        });
        log.debug(FINISH_CURRENT, accountId);
        return ticketCounter;
    }
}
