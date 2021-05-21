package ru.itterminal.yanmas.tickets.service.impl;

import org.springframework.stereotype.Service;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.yanmas.aau.service.CrudServiceWithBusinessHandlerImpl;
import ru.itterminal.yanmas.tickets.model.TicketEvent;
import ru.itterminal.yanmas.tickets.repository.TicketEventRepository;

@Slf4j
@Service
public class TicketEventServiceImpl extends CrudServiceWithBusinessHandlerImpl<TicketEvent, TicketEventRepository> {

}
