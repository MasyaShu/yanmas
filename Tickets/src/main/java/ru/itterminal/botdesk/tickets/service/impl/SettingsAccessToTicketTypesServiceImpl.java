package ru.itterminal.botdesk.tickets.service.impl;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.tickets.model.SettingsAccessToTicketTypes;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.repository.SettingsAccessToTicketTypesRepository;
import ru.itterminal.botdesk.tickets.service.validator.SettingsAccessToTicketTypesOperationValidator;

@SuppressWarnings("unused")
@Slf4j
@Service
@RequiredArgsConstructor
public class SettingsAccessToTicketTypesServiceImpl extends
        CrudServiceWithAccountImpl<SettingsAccessToTicketTypes, SettingsAccessToTicketTypesOperationValidator, SettingsAccessToTicketTypesRepository> {

    public static final String WHERE = "SettingsAccessToTicketTypesServiceImpl.findByUniqueFields: ";
    public static final String START_FIND = "Start " + WHERE + "{} , {}, {}";

    private final AccountServiceImpl accountService;
    private final GroupServiceImpl groupService;
    private final UserServiceImpl userService;
    private final GroupTicketTypesServiceImpl groupTicketTypesService;

    @Transactional(readOnly = true)
    public List<TicketType> getPermittedTicketTypes(UUID userId) {
        var foundSettings = findSettings(userId);
        return foundSettings.getGroupTicketTypes().getTicketTypes();
    }

    @Transactional(readOnly = true)
    public boolean isPermittedTicketType(UUID checkedTicketTypeId, UUID userId) {
        var foundSettings = findSettings(userId);
        if (foundSettings == null) {
            return true;
        }
        var ticketTypesIdList = foundSettings.getGroupTicketTypes().getTicketTypes().stream()
                .map(BaseEntity::getId)
                .collect(Collectors.toList());
        return ticketTypesIdList.contains(checkedTicketTypeId);
    }

    private SettingsAccessToTicketTypes findSettings(UUID userId) {
        var user = userService.findByIdAndAccountId(userId);
        var accountId = user.getAccount().getId();
        var groupId = user.getGroup().getId();
        var foundSettings =
                repository.getByAccount_IdAndGroup_IdAndUser_IdAndDeletedIsFalse(accountId, groupId, userId);
        if (foundSettings == null) {
            foundSettings =
                    repository.getByAccount_IdAndGroup_IdAndUserIsNullAndDeletedIsFalse(accountId, groupId);
        }
        if (foundSettings == null) {
            foundSettings =
                    repository.getByAccount_IdAndGroupIsNullAndUserIsNullAndDeletedIsFalse(accountId);
        }
        return foundSettings;
    }

    @Transactional(readOnly = true)
    public List<SettingsAccessToTicketTypes> findByUniqueFields(
            SettingsAccessToTicketTypes settingsAccess) {
        log.trace(START_FIND, settingsAccess.getAccount(), settingsAccess.getGroup(), settingsAccess.getUser());
        return repository.findAllByAccount_IdAndGroup_IdAndUser_IdAndIdNot(
                settingsAccess.getAccount().getId(),
                settingsAccess.getGroup() == null ? null : settingsAccess.getGroup().getId(),
                settingsAccess.getUser() == null ? null : settingsAccess.getUser().getId(),
                settingsAccess.getId()
        );
    }

    @Override
    protected void setNestedObjectsOfEntityBeforeCreate(SettingsAccessToTicketTypes entity) {
        entity.setAccount(accountService.findById(entity.getAccount().getId()));
        if (entity.getUser() != null && entity.getUser().getId() != null) {
            entity.setUser(userService.findByIdAndAccountId(entity.getUser().getId()));
            entity.setGroup(entity.getUser().getGroup());
        } else if (entity.getGroup() != null && entity.getGroup().getId() != null) {
            entity.setGroup(groupService.findByIdAndAccountId(entity.getGroup().getId()));
        }
        if (entity.getGroupTicketTypes() != null && entity.getGroupTicketTypes().getId() != null) {
            entity.setGroupTicketTypes(
                    groupTicketTypesService.findByIdAndAccountId(entity.getGroupTicketTypes().getId()));
        }
    }

    @Override
    protected void setNestedObjectsOfEntityBeforeUpdate(SettingsAccessToTicketTypes entity) {
        setNestedObjectsOfEntityBeforeCreate(entity);
    }

}
