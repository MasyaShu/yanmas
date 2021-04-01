package ru.itterminal.yanmas.tickets.service.impl;

import java.util.List;
import java.util.stream.Collectors;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.yanmas.aau.service.impl.AccountServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.tickets.model.GroupTicketTypes;
import ru.itterminal.yanmas.tickets.model.projection.GroupTicketTypesUniqueFields;
import ru.itterminal.yanmas.tickets.repository.GroupTicketTypesRepository;
import ru.itterminal.yanmas.tickets.service.validator.GroupTicketTypesOperationValidator;

@Slf4j
@Service
@RequiredArgsConstructor
public class GroupTicketTypesServiceImpl extends
        CrudServiceWithAccountImpl<GroupTicketTypes, GroupTicketTypesOperationValidator, GroupTicketTypesRepository> {

    public static final String START_FIND_GROUP_OF_TICKET_TYPES_BY_UNIQUE_FIELDS_NAME_ACCOUNT =
            "Start find group of ticket types by unique fields, name: {} account: {}";

    private final AccountServiceImpl accountService;
    private final TicketTypeServiceImpl ticketTypeService;

    @Transactional(readOnly = true)
    public List<GroupTicketTypesUniqueFields> findByUniqueFields(GroupTicketTypes groupTicketTypes) {
        log.trace(START_FIND_GROUP_OF_TICKET_TYPES_BY_UNIQUE_FIELDS_NAME_ACCOUNT,
                  groupTicketTypes.getName(), groupTicketTypes.getAccount()
        );
        return repository
                .getByNameAndAccount_IdAndIdNot(
                        groupTicketTypes.getName(),
                        groupTicketTypes.getAccount().getId(),
                        groupTicketTypes.getId()
                );
    }

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
