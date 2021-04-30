package ru.itterminal.yanmas.tickets.service.impl;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.impl.CrudServiceWithBusinessHandlerImpl;
import ru.itterminal.yanmas.aau.service.impl.WhoWatchedEntityServiceImpl;
import ru.itterminal.yanmas.tickets.model.TicketEvent;
import ru.itterminal.yanmas.tickets.repository.TicketEventRepository;
import ru.itterminal.yanmas.tickets.service.business_handler.TicketEventBusinessHandler;
import ru.itterminal.yanmas.tickets.service.validator.TicketEventOperationValidator;

@Slf4j
@Service
@RequiredArgsConstructor
public class TicketEventServiceImpl extends CrudServiceWithBusinessHandlerImpl<TicketEvent, TicketEventOperationValidator, TicketEventBusinessHandler, TicketEventRepository> {

    private final WhoWatchedEntityServiceImpl whoWatchedEntityService;

    public static final String YOU_MUST_USE_ANOTHER_METHOD_CREATE =
            "You must use another method Create(TicketEvent entity, User currentUser)";
    public static final String TICKET_EVENT_MUST_NOT_BE_UPDATED =
            "Ticket event must not be updated";

    @Override
    public TicketEvent create(TicketEvent entity, User currentUser) {
        return super.create(entity, currentUser);
    }
}
