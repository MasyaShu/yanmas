package ru.itterminal.botdesk.tickets.model.test;

import java.util.List;

import ru.itterminal.botdesk.aau.model.test.GroupTestHelper;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.commons.model.EntityTestHelperImpl;
import ru.itterminal.botdesk.tickets.model.SettingsAccessToTicketTypes;
import ru.itterminal.botdesk.tickets.model.dto.SettingsAccessToTicketTypesDtoRequest;
import ru.itterminal.botdesk.tickets.model.dto.SettingsAccessToTicketTypesDtoResponse;

public class SettingsAccessToTicketTypesTestHelper extends EntityTestHelperImpl<SettingsAccessToTicketTypes, SettingsAccessToTicketTypesDtoRequest,
        SettingsAccessToTicketTypesDtoResponse> {

    private final GroupTicketTypesTestHelper groupTicketTypesTestHelper = new GroupTicketTypesTestHelper();
    private final GroupTestHelper groupTestHelper = new GroupTestHelper();
    private final UserTestHelper userTestHelper = new UserTestHelper();

    @Override
    public SettingsAccessToTicketTypes getRandomValidEntity() {
        var groupTicketTypes = groupTicketTypesTestHelper.getRandomValidEntity();
        var account = groupTicketTypes.getAccount();
        var group = groupTestHelper.getRandomValidEntity();
        group.setAccount(account);
        var user = userTestHelper.getRandomValidEntity();
        user.setAccount(account);
        user.setGroup(group);
        var scenario = fakerEN.random().nextInt(0, 2);
        switch (scenario) {
            case 0 -> {
                group = null;
                user = null;
            }
            case 1 -> user = null;
        }
        var settingsAccessToTicketViaTicketTypes = SettingsAccessToTicketTypes.builder()
                .account(account)
                .group(group)
                .user(user)
                .groupTicketTypes(groupTicketTypes)
                .build();
        setRandomValidPropertiesOfBaseEntity(settingsAccessToTicketViaTicketTypes);
        return settingsAccessToTicketViaTicketTypes;
    }

    @Override
    public List<SettingsAccessToTicketTypes> setPredefinedValidEntityList() {
        return null;
    }

    @SuppressWarnings("DuplicatedCode")
    @Override
    public SettingsAccessToTicketTypesDtoRequest convertEntityToDtoRequest(SettingsAccessToTicketTypes entity, boolean isDtoForCreate) {
        var dtoRequest = SettingsAccessToTicketTypesDtoRequest.builder()
                .groupId(
                        entity.getGroup() == null
                                ? null
                                : entity.getGroup().getId()
                )
                .userId(
                        entity.getUser() == null
                                ? null
                                : entity.getUser().getId()
                )
                .groupTicketTypesId(
                        entity.getGroupTicketTypes() == null
                                ? null
                                : entity.getGroupTicketTypes().getId()
                )
                .displayName(null)
                .build();
        if (isDtoForCreate) {
            dtoRequest.setId(null);
            dtoRequest.setDeleted(null);
            dtoRequest.setVersion(null);
        } else {
            dtoRequest.setId(entity.getId());
            dtoRequest.setDeleted(entity.getDeleted());
            dtoRequest.setVersion(entity.getVersion());
        }
        return dtoRequest;
    }
}
