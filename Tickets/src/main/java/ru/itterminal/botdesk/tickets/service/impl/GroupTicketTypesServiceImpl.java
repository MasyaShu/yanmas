package ru.itterminal.botdesk.tickets.service.impl;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.commons.model.EntityConverter;
import ru.itterminal.botdesk.tickets.model.GroupTicketTypes;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.dto.GroupTicketTypesDtoRequest;
import ru.itterminal.botdesk.tickets.model.projection.GroupTicketTypesUniqueFields;
import ru.itterminal.botdesk.tickets.repository.GroupTicketTypesRepository;
import ru.itterminal.botdesk.tickets.service.validator.GroupTicketTypesOperationValidator;

@Slf4j
@Service
@RequiredArgsConstructor
public class GroupTicketTypesServiceImpl extends
        CrudServiceWithAccountImpl<GroupTicketTypes, GroupTicketTypesOperationValidator, GroupTicketTypesRepository>
        implements EntityConverter<GroupTicketTypes, GroupTicketTypesDtoRequest> {

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
    public GroupTicketTypes convertRequestDtoIntoEntityWithNestedObjectsWithOnlyId(
            GroupTicketTypesDtoRequest request,
            UUID accountId) {
        var groupTicketTypes = modelMapper.map(request, GroupTicketTypes.class);
        groupTicketTypes.setAccount(Account.builder().id(accountId).build());
        groupTicketTypes.setTicketTypes(
                request.getTicketTypes().stream()
                        .map(id -> TicketType.builder().id(id).build())
                        .collect(Collectors.toList())
        );
        return groupTicketTypes;
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
