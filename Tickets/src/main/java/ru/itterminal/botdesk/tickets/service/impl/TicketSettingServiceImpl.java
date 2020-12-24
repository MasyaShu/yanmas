package ru.itterminal.botdesk.tickets.service.impl;

import java.util.List;
import java.util.UUID;

import javax.validation.constraints.NotNull;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.projection.TicketSettingUniqueFields;
import ru.itterminal.botdesk.tickets.repository.TicketSettingRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketSettingOperationValidator;

@Slf4j
@Service
@Transactional
@RequiredArgsConstructor
public class TicketSettingServiceImpl extends CrudServiceWithAccountImpl<TicketSetting, TicketSettingOperationValidator,
        TicketSettingRepository> {

    public static final String WHERE = "TicketSettingServiceImpl.findByUniqueFields: ";
    public static final String START_FIND = "Start " + WHERE + "{} , {}, {}";

    private final TicketTypeServiceImpl ticketTypeService;
    private final TicketStatusServiceImpl ticketStatusService;

    @Transactional(readOnly = true)
    public List<TicketSettingUniqueFields> findByUniqueFields(TicketSetting ticketSetting) {
        log.trace(START_FIND, ticketSetting.getAccount(), ticketSetting.getGroup(), ticketSetting.getAuthor());
        return repository.findByUniqueFields(
                ticketSetting.getAccount().getId(),
                ticketSetting.getGroup() == null ? null : ticketSetting.getGroup().getId(),
                ticketSetting.getAuthor() == null ? null : ticketSetting.getAuthor().getId(),
                ticketSetting.getId()
        );
    }

    @Transactional(readOnly = true)
    public TicketSetting getSettingOrPredefinedValuesForTicket(@NotNull UUID accountId, @NotNull UUID groupId,
                                                               @NotNull UUID authorId) {
        var ticketSetting = repository.findByUniqueFields(accountId, groupId, authorId, null);
        if (ticketSetting.isEmpty()) {
            ticketSetting = repository.findByUniqueFields(accountId, groupId, null, null);
        }
        if (ticketSetting.isEmpty()) {
            ticketSetting = repository.findByUniqueFields(accountId, null, null, null);
        }
        var valuesForTicket =
                ticketSetting.isEmpty() ? new TicketSetting() : repository.findById(ticketSetting.get(0).getId()).get();
        if (valuesForTicket.getTicketTypeForNew() == null) {
            var predefinedTicketTypeForNew =
                    ticketTypeService.findByIdAndAccountId(TicketType.PREDEFINED_TICKET_TYPE_FOR_NEW_ID, accountId);
            valuesForTicket.setTicketTypeForNew(predefinedTicketTypeForNew);
        }
        if (valuesForTicket.getTicketStatusForNew() == null) {
            var predefinedTicketStatusForNew =
                    ticketStatusService
                            .findByIdAndAccountId(TicketStatus.PREDEFINED_TICKET_STATUS_FOR_NEW_ID, accountId);
            valuesForTicket.setTicketStatusForNew(predefinedTicketStatusForNew);
        }
        if (valuesForTicket.getTicketStatusForReopen() == null) {
            var predefinedTicketStatusForReopen =
                    ticketStatusService.findByIdAndAccountId(
                            TicketStatus.PREDEFINED_TICKET_STATUS_FOR_REOPEN_ID, accountId);
            valuesForTicket.setTicketStatusForReopen(predefinedTicketStatusForReopen);
        }
        if (valuesForTicket.getTicketStatusForClose() == null) {
            var predefinedTicketStatusForClose =
                    ticketStatusService.findByIdAndAccountId(
                            TicketStatus.PREDEFINED_TICKET_STATUS_FOR_CLOSE_ID, accountId);
            valuesForTicket.setTicketStatusForClose(predefinedTicketStatusForClose);
        }
        if (valuesForTicket.getTicketStatusForCancel() == null) {
            var predefinedTicketStatusForCancel =
                    ticketStatusService.findByIdAndAccountId(
                            TicketStatus.PREDEFINED_TICKET_STATUS_FOR_CANCEL_ID, accountId);
            valuesForTicket.setTicketStatusForCancel(predefinedTicketStatusForCancel);
        }
        return valuesForTicket;
    }

}
