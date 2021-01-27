package ru.itterminal.botdesk.tickets.service.impl;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.projection.TicketSettingUniqueFields;
import ru.itterminal.botdesk.tickets.repository.TicketSettingRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketSettingOperationValidator;

import javax.validation.constraints.NotNull;
import java.util.List;
import java.util.UUID;

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
        var ticketSettingProjection = repository.findByUniqueFieldsWithoutDeleted(accountId, groupId, authorId, null);
        if (ticketSettingProjection.isEmpty()) {
            ticketSettingProjection = repository.findByUniqueFieldsWithoutDeleted(accountId, groupId, null, null);
        }
        if (ticketSettingProjection.isEmpty()) {
            ticketSettingProjection = repository.findByUniqueFieldsWithoutDeleted(accountId, null, null, null);
        }
        var valuesForTicket =
                ticketSettingProjection.isEmpty() ? new TicketSetting() : repository.findById(ticketSettingProjection.get(0).getId()).get();
        if (valuesForTicket.getTicketTypeForNew() == null) {
            var predefinedTicketTypeForNew =
                    ticketTypeService
                            .findStartedPredefinedTicketTypeForNewTicket(accountId);
            valuesForTicket.setTicketTypeForNew(predefinedTicketTypeForNew);
        }
        if (valuesForTicket.getTicketStatusForNew() == null) {
            var predefinedTicketStatusForNew =
                    ticketStatusService
                            .findStartedPredefinedStatus(accountId);
            valuesForTicket.setTicketStatusForNew(predefinedTicketStatusForNew);
        }
        if (valuesForTicket.getTicketStatusForReopen() == null) {
            var predefinedTicketStatusForReopen =
                    ticketStatusService
                            .findReopenedPredefinedStatus(accountId);
            valuesForTicket.setTicketStatusForReopen(predefinedTicketStatusForReopen);
        }
        if (valuesForTicket.getTicketStatusForClose() == null) {
            var predefinedTicketStatusForClose =
                    ticketStatusService
                            .findFinishedPredefinedStatus(accountId);
            valuesForTicket.setTicketStatusForClose(predefinedTicketStatusForClose);
        }
        if (valuesForTicket.getTicketStatusForCancel() == null) {
            var predefinedTicketStatusForCancel =
                    ticketStatusService
                            .findCanceledPredefinedStatus(accountId);
            valuesForTicket.setTicketStatusForCancel(predefinedTicketStatusForCancel);
        }
        return valuesForTicket;
    }

}
