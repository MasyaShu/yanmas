package ru.itterminal.yanmas.tickets.service.impl;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import ru.itterminal.yanmas.aau.service.impl.AccountServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.tickets.model.GroupTicketTypes;
import ru.itterminal.yanmas.tickets.repository.GroupTicketTypesRepository;
import ru.itterminal.yanmas.tickets.service.validator.GroupTicketTypesOperationValidator;

import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class GroupTicketTypesServiceImpl extends
        CrudServiceWithAccountImpl<GroupTicketTypes, GroupTicketTypesOperationValidator, GroupTicketTypesRepository> {

    private final AccountServiceImpl accountService;
    private final TicketTypeServiceImpl ticketTypeService;

    @Override
    protected void setNestedObjectsOfEntityBeforeCreate(GroupTicketTypes entity) {
        entity.setAccount(accountService.findById(entity.getAccount().getId()));
        entity.setTicketTypes(
                ticketTypeService.findAllByAccountIdAndListId(
                        entity.getTicketTypes().stream()
                                .map(BaseEntity::getId)
                                .collect(Collectors.toList())
                )
        );
    }

    @Override
    protected void setNestedObjectsOfEntityBeforeUpdate(GroupTicketTypes entity) {
        setNestedObjectsOfEntityBeforeCreate(entity);
    }
}
