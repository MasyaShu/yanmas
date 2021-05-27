package ru.itterminal.yanmas.tickets.service.impl;

import javax.validation.constraints.NotNull;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.CrudServiceWithBusinessHandlerImpl;
import ru.itterminal.yanmas.tickets.model.TicketSetting;
import ru.itterminal.yanmas.tickets.repository.TicketSettingRepository;
import ru.itterminal.yanmas.tickets.service.validator.ticket_setting.TicketSettingOperationValidator;

@Service
@Transactional
@RequiredArgsConstructor
public class TicketSettingServiceImpl extends CrudServiceWithBusinessHandlerImpl
        <TicketSetting, TicketSettingRepository> {

    private final TicketStatusServiceImpl ticketStatusService;
    private final TicketSettingOperationValidator validator;
    private final TicketTypeServiceImpl ticketTypeService;
    private final SettingsAccessToTicketTypesServiceImpl settingsAccessToTicketTypesService;

    @Transactional(readOnly = true)
    public TicketSetting getSettingOrPredefinedValuesForTicket(@NotNull User currentUser,
                                                               @NotNull User authorOfTicket) {
        validator.checkAccessForGetSettingOrPredefinedValuesForTicket(currentUser, authorOfTicket);
        var ticketSetting = repository
                .getByAccount_IdAndGroup_IdAndAuthor_IdAndDeletedIsFalse(currentUser.getAccount().getId(), authorOfTicket.getGroup().getId(), authorOfTicket.getId());
        if (ticketSetting == null) {
            ticketSetting = repository
                    .getByAccount_IdAndGroup_IdAndAuthorIsNullAndDeletedIsFalse(currentUser.getAccount().getId(), authorOfTicket.getGroup().getId());
        }
        if (ticketSetting == null) {
            ticketSetting = repository
                    .getByAccount_IdAndGroupIsNullAndAuthorIsNullAndDeletedIsFalse(currentUser.getAccount().getId());
        }
        if (ticketSetting == null) {
            ticketSetting = new TicketSetting();
        }
        if (ticketSetting.getTicketTypeForNew() != null) {
            var ticketTypeId = ticketSetting.getTicketTypeForNew().getId();
            if (!settingsAccessToTicketTypesService.isPermittedTicketType(ticketTypeId, authorOfTicket.getId())) {
                ticketSetting.setTicketTypeForNew(null);
            }
        }
        if (ticketSetting.getTicketTypeForNew() == null) {
            var predefinedTicketTypeForNew =
                    ticketTypeService
                            .findStartedPredefinedTicketTypeForNewTicket(currentUser.getAccount().getId());
            ticketSetting.setTicketTypeForNew(predefinedTicketTypeForNew);
        }
        if (ticketSetting.getTicketStatusForNew() == null) {
            var predefinedTicketStatusForNew =
                    ticketStatusService
                            .findStartedPredefinedStatus(currentUser.getAccount().getId());
            ticketSetting.setTicketStatusForNew(predefinedTicketStatusForNew);
        }
        if (ticketSetting.getTicketStatusForReopen() == null) {
            var predefinedTicketStatusForReopen =
                    ticketStatusService
                            .findReopenedPredefinedStatus(currentUser.getAccount().getId());
            ticketSetting.setTicketStatusForReopen(predefinedTicketStatusForReopen);
        }
        if (ticketSetting.getTicketStatusForClose() == null) {
            var predefinedTicketStatusForClose =
                    ticketStatusService
                            .findFinishedPredefinedStatus(currentUser.getAccount().getId());
            ticketSetting.setTicketStatusForClose(predefinedTicketStatusForClose);
        }
        if (ticketSetting.getTicketStatusForCancel() == null) {
            var predefinedTicketStatusForCancel =
                    ticketStatusService
                            .findCanceledPredefinedStatus(currentUser.getAccount().getId());
            ticketSetting.setTicketStatusForCancel(predefinedTicketStatusForCancel);
        }
        return ticketSetting;
    }

}
