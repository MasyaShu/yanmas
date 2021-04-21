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
import ru.itterminal.yanmas.tickets.model.TicketSetting;
import ru.itterminal.yanmas.tickets.repository.TicketSettingRepository;
import ru.itterminal.yanmas.tickets.service.validator.TicketSettingOperationValidator;

import javax.validation.constraints.NotNull;
import java.util.UUID;
import java.util.stream.Collectors;

@Slf4j
@Service
@Transactional
@RequiredArgsConstructor
public class TicketSettingServiceImpl extends CrudServiceWithAccountImpl<TicketSetting, TicketSettingOperationValidator,
        TicketSettingRepository> {

    private final AccountServiceImpl accountService;
    private final GroupServiceImpl groupService;
    private final UserServiceImpl userService;
    private final TicketStatusServiceImpl ticketStatusService;
    private final TicketTypeServiceImpl ticketTypeService;
    private final SettingsAccessToTicketTypesServiceImpl settingsAccessToTicketTypesService;

    @Transactional(readOnly = true)
    public TicketSetting getSettingOrPredefinedValuesForTicket(@NotNull UUID accountId,
                                                               @NotNull UUID groupId,
                                                               @NotNull UUID authorId) {
        validator.checkAccessForGetSettingOrPredefinedValuesForTicket(groupId);
        var ticketSetting = repository
                .getByAccount_IdAndGroup_IdAndAuthor_IdAndDeletedIsFalse(accountId, groupId, authorId);
        if (ticketSetting == null) {
            ticketSetting = repository
                    .getByAccount_IdAndGroup_IdAndAuthorIsNullAndDeletedIsFalse(accountId, groupId);
        }
        if (ticketSetting == null) {
            ticketSetting = repository
                    .getByAccount_IdAndGroupIsNullAndAuthorIsNullAndDeletedIsFalse(accountId);
        }
        if (ticketSetting == null) {
            ticketSetting = new TicketSetting();
        }
        if (ticketSetting.getTicketTypeForNew() != null) {
            var ticketTypeId = ticketSetting.getTicketTypeForNew().getId();
            if (!settingsAccessToTicketTypesService.isPermittedTicketType(ticketTypeId, authorId)) {
                ticketSetting.setTicketTypeForNew(null);
            }
        }
        if (ticketSetting.getTicketTypeForNew() == null) {
            var predefinedTicketTypeForNew =
                    ticketTypeService
                            .findStartedPredefinedTicketTypeForNewTicket(accountId);
            ticketSetting.setTicketTypeForNew(predefinedTicketTypeForNew);
        }
        if (ticketSetting.getTicketStatusForNew() == null) {
            var predefinedTicketStatusForNew =
                    ticketStatusService
                            .findStartedPredefinedStatus(accountId);
            ticketSetting.setTicketStatusForNew(predefinedTicketStatusForNew);
        }
        if (ticketSetting.getTicketStatusForReopen() == null) {
            var predefinedTicketStatusForReopen =
                    ticketStatusService
                            .findReopenedPredefinedStatus(accountId);
            ticketSetting.setTicketStatusForReopen(predefinedTicketStatusForReopen);
        }
        if (ticketSetting.getTicketStatusForClose() == null) {
            var predefinedTicketStatusForClose =
                    ticketStatusService
                            .findFinishedPredefinedStatus(accountId);
            ticketSetting.setTicketStatusForClose(predefinedTicketStatusForClose);
        }
        if (ticketSetting.getTicketStatusForCancel() == null) {
            var predefinedTicketStatusForCancel =
                    ticketStatusService
                            .findCanceledPredefinedStatus(accountId);
            ticketSetting.setTicketStatusForCancel(predefinedTicketStatusForCancel);
        }
        return ticketSetting;
    }

    @Override
    protected void setNestedObjectsOfEntityBeforeCreate(TicketSetting entity) {
        entity.setAccount(accountService.findById(entity.getAccount().getId()));
        if (entity.getAuthor() != null && entity.getAuthor().getId() != null) {
            entity.setAuthor(userService.findByIdAndAccountId(entity.getAuthor().getId()));
            entity.setGroup(entity.getAuthor().getGroup());
        } else if (entity.getGroup() != null && entity.getGroup().getId() != null) {
            entity.setGroup(groupService.findByIdAndAccountId(entity.getGroup().getId()));
        }
        if (entity.getObservers() != null) {
            entity.setObservers(
                    userService.findAllByAccountIdAndListId(
                            entity.getObservers().stream()
                                    .map(BaseEntity::getId)
                                    .collect(Collectors.toList())
                    )
            );
        }
        if (entity.getExecutors() != null) {
            entity.setExecutors(
                    userService.findAllByAccountIdAndListId(
                            entity.getExecutors().stream()
                                    .map(BaseEntity::getId)
                                    .collect(Collectors.toList())
                    )
            );
        }
        if (entity.getTicketTypeForNew() != null && entity.getTicketTypeForNew().getId() != null) {
            entity.setTicketTypeForNew(ticketTypeService.findByIdAndAccountId(entity.getTicketTypeForNew().getId()));
        }
        if (entity.getTicketStatusForNew() != null && entity.getTicketStatusForNew().getId() != null) {
            entity.setTicketStatusForNew(
                    ticketStatusService.findByIdAndAccountId(entity.getTicketStatusForNew().getId())
            );
        }
        if (entity.getTicketStatusForReopen() != null && entity.getTicketStatusForReopen().getId() != null) {
            entity.setTicketStatusForReopen(
                    ticketStatusService.findByIdAndAccountId(entity.getTicketStatusForReopen().getId())
            );
        }
        if (entity.getTicketStatusForClose() != null && entity.getTicketStatusForClose().getId() != null) {
            entity.setTicketStatusForClose(
                    ticketStatusService.findByIdAndAccountId(entity.getTicketStatusForClose().getId())
            );
        }
        if (entity.getTicketStatusForCancel() != null && entity.getTicketStatusForCancel().getId() != null) {
            entity.setTicketStatusForCancel(
                    ticketStatusService.findByIdAndAccountId(entity.getTicketStatusForCancel().getId())
            );
        }
    }

    @Override
    protected void setNestedObjectsOfEntityBeforeUpdate(TicketSetting entity) {
        setNestedObjectsOfEntityBeforeCreate(entity);
    }

}
