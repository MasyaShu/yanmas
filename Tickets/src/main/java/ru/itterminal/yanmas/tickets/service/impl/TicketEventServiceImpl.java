package ru.itterminal.yanmas.tickets.service.impl;

import java.util.List;
import java.util.UUID;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.yanmas.aau.service.impl.WhoWatchedEntityServiceImpl;
import ru.itterminal.yanmas.tickets.model.TicketEvent;
import ru.itterminal.yanmas.tickets.repository.TicketEventRepository;
import ru.itterminal.yanmas.tickets.service.validator.TicketEventOperationValidator;

@Slf4j
@Service
@RequiredArgsConstructor
public class TicketEventServiceImpl extends CrudServiceWithAccountImpl<TicketEvent, TicketEventOperationValidator, TicketEventRepository>{

    private final WhoWatchedEntityServiceImpl whoWatchedEntityService;

    public static final String YOU_MUST_USE_ANOTHER_METHOD_CREATE =
            "You must use another method Create(TicketEvent entity, User currentUser)";
    public static final String TICKET_EVENT_MUST_NOT_BE_UPDATED =
            "Ticket event must not be updated";

    @Override
    @Deprecated
    public TicketEvent create(TicketEvent entity) {
        throw new UnsupportedOperationException(YOU_MUST_USE_ANOTHER_METHOD_CREATE);
    }

    @Override
    @Deprecated
    public TicketEvent update(TicketEvent entity) {
        throw new UnsupportedOperationException(TICKET_EVENT_MUST_NOT_BE_UPDATED);
    }

    @Transactional
    public TicketEvent create(TicketEvent entity, User currentUser) {
        var createdTicketEvent = entity; // TODO rewrite
        whoWatchedEntityService.watched(List.of(createdTicketEvent.getId()));
        return createdTicketEvent;
    }

    @Override
    public TicketEvent findByIdAndAccountId(UUID id) {
        var foundTicketEvent =  super.findByIdAndAccountId(id);
        whoWatchedEntityService.watched(List.of(foundTicketEvent.getId()));
        return foundTicketEvent;
    }


}
