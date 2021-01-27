package ru.itterminal.botdesk.tickets.service.impl;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.tickets.model.Ticket;
import ru.itterminal.botdesk.tickets.repository.TicketRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator;

@Slf4j
@Service
@RequiredArgsConstructor
public class TicketServiceImpl extends CrudServiceWithAccountImpl<Ticket, TicketOperationValidator, TicketRepository> {

    private final TicketCounterServiceImpl ticketCounterService;

    @SuppressWarnings("unused")
    @Transactional
    public Ticket create(Ticket entity, User creator) {
        var number = ticketCounterService.getTicketNumber(entity.getAccount().getId());
        entity.setNumber(number);
        return super.create(entity);
    }

}
