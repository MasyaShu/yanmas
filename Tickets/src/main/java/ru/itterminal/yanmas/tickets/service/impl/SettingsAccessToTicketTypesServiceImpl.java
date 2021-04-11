package ru.itterminal.yanmas.tickets.service.impl;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.yanmas.aau.service.impl.AccountServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.yanmas.aau.service.impl.GroupServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.tickets.model.SettingsAccessToTicketTypes;
import ru.itterminal.yanmas.tickets.model.TicketType;
import ru.itterminal.yanmas.tickets.repository.SettingsAccessToTicketTypesRepository;
import ru.itterminal.yanmas.tickets.service.validator.SettingsAccessToTicketTypesOperationValidator;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@SuppressWarnings("unused")
@Slf4j
@Service
@RequiredArgsConstructor
public class SettingsAccessToTicketTypesServiceImpl extends
        CrudServiceWithAccountImpl<SettingsAccessToTicketTypes, SettingsAccessToTicketTypesOperationValidator, SettingsAccessToTicketTypesRepository> {

    private final AccountServiceImpl accountService;
    private final GroupServiceImpl groupService;
    private final UserServiceImpl userService;
    private final GroupTicketTypesServiceImpl groupTicketTypesService;

    @Transactional(readOnly = true)
    public List<TicketType> getPermittedTicketTypes(UUID userId) {
        var foundSettings = findSettings(userId);
        if (foundSettings == null) {
            return null;
        }
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
