package ru.itterminal.yanmas.tickets.service.impl;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.yanmas.aau.service.business_handler.impl.CrudServiceWithBusinessHandlerImpl;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.tickets.model.SettingsAccessToTicketTypes;
import ru.itterminal.yanmas.tickets.model.TicketType;
import ru.itterminal.yanmas.tickets.repository.SettingsAccessToTicketTypesRepository;
import ru.itterminal.yanmas.tickets.service.business_handler.SettingsAccessToTicketTypesBusinessHandler;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class SettingsAccessToTicketTypesServiceImpl extends
        CrudServiceWithBusinessHandlerImpl<
                SettingsAccessToTicketTypes,
                SettingsAccessToTicketTypesBusinessHandler,
                SettingsAccessToTicketTypesRepository> {

    private final UserServiceImpl userService;

    @Transactional(readOnly = true)
    public List<TicketType> getPermittedTicketTypes(UUID userId) {
        var foundSettings = findSettings(userId);
        if (foundSettings == null) {
            return null; //NOSONAR
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
        var user = userService.findById(userId);
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


}
