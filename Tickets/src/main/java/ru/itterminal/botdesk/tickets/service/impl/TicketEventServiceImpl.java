package ru.itterminal.botdesk.tickets.service.impl;

import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.tickets.model.TicketEvent;
import ru.itterminal.botdesk.tickets.repository.TicketEventRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketEventOperationValidator;

@Slf4j
@Service
@RequiredArgsConstructor
public class TicketEventServiceImpl extends CrudServiceWithAccountImpl<TicketEvent, TicketEventOperationValidator, TicketEventRepository>{
}
