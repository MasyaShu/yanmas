package ru.itterminal.botdesk.tickets.service.impl;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import ru.itterminal.botdesk.aau.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.commons.model.EntityConverter;
import ru.itterminal.botdesk.tickets.model.TicketEvent;
import ru.itterminal.botdesk.tickets.model.dto.TicketEventDtoRequest;
import ru.itterminal.botdesk.tickets.repository.TicketEventRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketEventOperationValidator;

import java.util.UUID;

@Slf4j
@Service
@RequiredArgsConstructor
public class TicketEventServiceImpl extends CrudServiceWithAccountImpl<TicketEvent, TicketEventOperationValidator, TicketEventRepository>
        implements EntityConverter<TicketEvent, TicketEventDtoRequest> {
    @Override
    public TicketEvent convertRequestDtoIntoEntityWithNestedObjectsWithOnlyId(TicketEventDtoRequest request, UUID accountId) {
        return null;
    }

}
