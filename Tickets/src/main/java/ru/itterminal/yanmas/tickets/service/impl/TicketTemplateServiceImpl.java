package ru.itterminal.yanmas.tickets.service.impl;

import org.springframework.stereotype.Service;
import ru.itterminal.yanmas.aau.service.business_handler.impl.CrudServiceWithBusinessHandlerImpl;
import ru.itterminal.yanmas.tickets.model.TicketTemplate;
import ru.itterminal.yanmas.tickets.repository.TicketTemplateRepository;
import ru.itterminal.yanmas.tickets.service.business_handler.TicketTemplateBusinessHandler;

@Service
public class TicketTemplateServiceImpl extends CrudServiceWithBusinessHandlerImpl
        <TicketTemplate, TicketTemplateBusinessHandler, TicketTemplateRepository> {

}
