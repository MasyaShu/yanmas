package ru.itterminal.yanmas.tickets.service.impl;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import ru.itterminal.yanmas.aau.service.business_handler.impl.CrudServiceWithBusinessHandlerImpl;
import ru.itterminal.yanmas.tickets.model.TicketEvent;
import ru.itterminal.yanmas.tickets.repository.TicketEventRepository;
import ru.itterminal.yanmas.tickets.service.business_handler.TicketEventBusinessHandler;

@Slf4j
@Service
public class TicketEventServiceImpl extends CrudServiceWithBusinessHandlerImpl<TicketEvent, TicketEventBusinessHandler, TicketEventRepository> {

}
