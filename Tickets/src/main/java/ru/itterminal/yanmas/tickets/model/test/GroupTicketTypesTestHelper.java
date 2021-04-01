package ru.itterminal.yanmas.tickets.model.test;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.commons.model.EntityTestHelperImpl;
import ru.itterminal.yanmas.tickets.model.GroupTicketTypes;
import ru.itterminal.yanmas.tickets.model.dto.GroupTicketTypesDtoRequest;
import ru.itterminal.yanmas.tickets.model.dto.GroupTicketTypesDtoResponse;
@SuppressWarnings("DuplicatedCode")
public class GroupTicketTypesTestHelper extends EntityTestHelperImpl<GroupTicketTypes, GroupTicketTypesDtoRequest,
        GroupTicketTypesDtoResponse> {

    private final TicketTypeTestHelper ticketTypeTestHelper = new TicketTypeTestHelper();

    @Override
    public GroupTicketTypes getRandomValidEntity() {
        var listTicketTypes  =ticketTypeTestHelper.getRandomValidEntityList(2);
        var groupTicketTypes = GroupTicketTypes.builder()
                .account(listTicketTypes.get(0).getAccount())
                .name(fakerRU.name().firstName())
                .ticketTypes(listTicketTypes)
                .build();
        setRandomValidPropertiesOfBaseEntity(groupTicketTypes);
        return groupTicketTypes;
    }

    @Override
    public List<GroupTicketTypes> setPredefinedValidEntityList() {
        return null;
    }

    @Override
    public GroupTicketTypesDtoRequest convertEntityToDtoRequest(GroupTicketTypes entity, boolean isDtoForCreate) {
        var groupTicketTypesDtoRequest = GroupTicketTypesDtoRequest.builder()
                .name(entity.getName())
                .build();
        if (isDtoForCreate) {
            groupTicketTypesDtoRequest.setId(null);
            groupTicketTypesDtoRequest.setDeleted(null);
            groupTicketTypesDtoRequest.setVersion(null);
        } else {
            groupTicketTypesDtoRequest.setId(entity.getId());
            groupTicketTypesDtoRequest.setDeleted(entity.getDeleted());
            groupTicketTypesDtoRequest.setVersion(entity.getVersion());
        }
        groupTicketTypesDtoRequest.setDisplayName(null);
        List<UUID> ticketTypesIdList = new ArrayList<>();
        if (entity.getTicketTypes() != null) {
            ticketTypesIdList = entity.getTicketTypes()
                    .stream()
                    .map(BaseEntity::getId)
                    .collect(Collectors.toList());
        }
        groupTicketTypesDtoRequest.setTicketTypes(ticketTypesIdList);
        return groupTicketTypesDtoRequest;
    }
}
