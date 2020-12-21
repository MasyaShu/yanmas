package ru.itterminal.botdesk.tickets.model.test;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.test.AccountTestHelper;
import ru.itterminal.botdesk.aau.model.test.GroupTestHelper;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.commons.model.EntityTestHelperImpl;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.dto.TicketSettingDtoRequest;
import ru.itterminal.botdesk.tickets.model.dto.TicketSettingDtoResponse;

public class TicketSettingTestHelper extends EntityTestHelperImpl<TicketSetting, TicketSettingDtoRequest,
        TicketSettingDtoResponse> {

    private final AccountTestHelper accountTestHelper = new AccountTestHelper();
    private final UserTestHelper userTestHelper = new UserTestHelper();
    private final GroupTestHelper groupTestHelper = new GroupTestHelper();
    private final TicketTypeTestHelper ticketTypeTestHelper = new TicketTypeTestHelper();
    private final TicketStatusTestHelper ticketStatusTestHelper = new TicketStatusTestHelper();

    @Override
    public TicketSetting getRandomValidEntity() {
        Account account = accountTestHelper.getRandomValidEntity();

        Group group = groupTestHelper.getRandomValidEntity();
        group.setAccount(account);

        User author = userTestHelper.getRandomValidEntity();
        author.setAccount(account);
        author.setGroup(group);

        List<User> observers = userTestHelper.getRandomValidEntityList(5);
        for (User user : observers) {
            user.setAccount(account);
        }

        List<User> executors = userTestHelper.getRandomValidEntityList(5);
        for (User user : executors) {
            user.setAccount(account);
        }

        TicketType ticketType = ticketTypeTestHelper.getRandomValidEntity();
        ticketType.setAccount(account);

        TicketStatus ticketStatusForNew = ticketStatusTestHelper.getRandomValidEntity();
        ticketStatusForNew.setAccount(account);

        TicketStatus ticketStatusForReopen = ticketStatusTestHelper.getRandomValidEntity();
        ticketStatusForReopen.setAccount(account);

        TicketStatus ticketStatusForClose = ticketStatusTestHelper.getRandomValidEntity();
        ticketStatusForClose.setAccount(account);

        TicketStatus ticketStatusForCancel = ticketStatusTestHelper.getRandomValidEntity();
        ticketStatusForCancel.setAccount(account);

        TicketSetting ticketSetting = TicketSetting.builder()
                .account(account)
                .author(fakerRU.bool().bool() ? null : author)
                .group(fakerRU.bool().bool() ? null : group)
                .observers(fakerRU.bool().bool() ? null : observers)
                .executors(fakerRU.bool().bool() ? null : executors)
                .ticketTypeForNew(fakerRU.bool().bool() ? null : ticketType)
                .ticketStatusForNew(fakerRU.bool().bool() ? null : ticketStatusForNew)
                .ticketStatusForReopen(fakerRU.bool().bool() ? null : ticketStatusForReopen)
                .ticketStatusForClose(fakerRU.bool().bool() ? null : ticketStatusForClose)
                .ticketStatusForCancel(fakerRU.bool().bool() ? null : ticketStatusForCancel)
                .build();
        ticketSetting.generateDisplayName();
        setRandomValidPropertiesOfBaseEntity(ticketSetting);
        return ticketSetting;
    }

    @Override
    public List<TicketSetting> setPredefinedValidEntityList() {
        List<User> observers1 = new ArrayList<>();
        observers1.add(userTestHelper
                               .getEntityFromPredefinedValidEntityByEntityId("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"));
        observers1.add(userTestHelper
                               .getEntityFromPredefinedValidEntityByEntityId("e14d9ffd-0071-4c0e-99ed-932f007963f0"));

        List<User> executors1 = new ArrayList<>();
        executors1.add(userTestHelper
                               .getEntityFromPredefinedValidEntityByEntityId("d592facb-e6ee-4801-8310-9c7708eb6e6c"));
        executors1.add(userTestHelper
                               .getEntityFromPredefinedValidEntityByEntityId("cdfa6483-0769-4628-ba32-efd338a716de"));

        TicketSetting ticketSetting1 = TicketSetting.builder()
                .account(accountTestHelper
                                 .getEntityFromPredefinedValidEntityByEntityId("cdfa6483-0769-4628-ba32-efd338a716de"))
                .group(groupTestHelper
                               .getEntityFromPredefinedValidEntityByEntityId("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"))
                .author(userTestHelper
                                .getEntityFromPredefinedValidEntityByEntityId("d592facb-e6ee-4801-8310-9c7708eb6e6c"))
                .observers(observers1)
                .executors(executors1)
                .ticketTypeForNew(ticketTypeTestHelper.getEntityFromPredefinedValidEntityByEntityId(
                        "7f66b241-f8ec-4912-8f58-a4ceef2dd4c9"))
                .ticketStatusForNew(ticketStatusTestHelper.getEntityFromPredefinedValidEntityByEntityId(
                        "7f66b241-f8ec-4912-8f58-a4ceef2dd4c9"))
                .ticketStatusForReopen(ticketStatusTestHelper.getEntityFromPredefinedValidEntityByEntityId(
                        "17b13694-1907-4af9-8f5d-bfa444356e73"))
                .ticketStatusForClose(ticketStatusTestHelper.getEntityFromPredefinedValidEntityByEntityId(
                        "17b13694-1907-4af9-8f5d-bfa444356e73"))
                .ticketStatusForCancel(ticketStatusTestHelper.getEntityFromPredefinedValidEntityByEntityId(
                        "17b13694-1907-4af9-8f5d-bfa444356e73"))
                .build();
        setPropertiesOfBaseEntity(ticketSetting1, UUID.fromString("9c8183ba-5d13-442f-a741-5b3134a3c140"),
                                  0, false, null
        );
        return List.of(ticketSetting1);
    }

    @Override
    public TicketSettingDtoRequest convertEntityToDtoRequest(TicketSetting entity) {

        List<UUID> observersIdList = entity.getObservers()
                .stream()
                .map(BaseEntity::getId)
                .collect(Collectors.toList());

        List<UUID> executorsIdList = entity.getExecutors()
                .stream()
                .map(BaseEntity::getId)
                .collect(Collectors.toList());

        return TicketSettingDtoRequest.builder()
                .group(entity.getGroup() == null
                               ? null
                               : entity.getGroup().getId())
                .author(entity.getAuthor() == null
                                ? null
                                : entity.getAuthor().getId())
                .observers(observersIdList)
                .executors(executorsIdList)
                .ticketTypeForNew(entity.getTicketTypeForNew() == null
                                          ? null
                                          : entity.getTicketTypeForNew().getId())
                .ticketStatusForNew(entity.getTicketStatusForNew() == null
                                            ? null
                                            : entity.getTicketStatusForNew().getId())
                .ticketStatusForReopen(entity.getTicketStatusForReopen() == null
                                               ? null
                                               : entity.getTicketStatusForReopen().getId())
                .ticketStatusForCancel(entity.getTicketStatusForCancel() == null
                                               ? null
                                               : entity.getTicketStatusForCancel().getId())
                .ticketStatusForClose(entity.getTicketStatusForClose() == null
                                              ? null
                                              : entity.getTicketStatusForClose().getId())
                .build();
    }
}
