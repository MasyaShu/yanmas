package ru.itterminal.yanmas.tickets.service.impl;

import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.yanmas.aau.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.yanmas.tickets.model.TicketEvent;
import ru.itterminal.yanmas.tickets.repository.TicketEventRepository;
import ru.itterminal.yanmas.tickets.service.validator.TicketEventOperationValidator;

@Slf4j
@Service
@RequiredArgsConstructor
public class TicketEventServiceImpl extends CrudServiceWithAccountImpl<TicketEvent, TicketEventOperationValidator, TicketEventRepository>{
}
