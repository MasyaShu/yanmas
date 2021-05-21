package ru.itterminal.yanmas.tickets.service.impl;

import org.springframework.stereotype.Service;

import ru.itterminal.yanmas.aau.service.CrudServiceWithBusinessHandlerImpl;
import ru.itterminal.yanmas.tickets.model.GroupTicketTypes;
import ru.itterminal.yanmas.tickets.repository.GroupTicketTypesRepository;

@Service
public class GroupTicketTypesServiceImpl extends
        CrudServiceWithBusinessHandlerImpl<GroupTicketTypes, GroupTicketTypesRepository> {

}
