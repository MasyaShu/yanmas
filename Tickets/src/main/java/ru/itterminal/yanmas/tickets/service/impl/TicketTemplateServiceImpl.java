package ru.itterminal.yanmas.tickets.service.impl;

import org.springframework.stereotype.Service;

import ru.itterminal.yanmas.aau.service.CrudServiceWithBusinessHandlerImpl;
import ru.itterminal.yanmas.tickets.model.TicketTemplate;
import ru.itterminal.yanmas.tickets.repository.TicketTemplateRepository;

@Service
public class TicketTemplateServiceImpl extends CrudServiceWithBusinessHandlerImpl
        <TicketTemplate, TicketTemplateRepository> {
}
