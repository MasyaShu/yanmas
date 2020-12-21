package ru.itterminal.botdesk.tickets.service.impl;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.projection.TicketSettingUniqueFields;
import ru.itterminal.botdesk.tickets.repository.TicketSettingRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketSettingOperationValidator;

@Slf4j
@Service
@Transactional
public class TicketSettingServiceImpl extends CrudServiceWithAccountImpl<TicketSetting, TicketSettingOperationValidator,
        TicketSettingRepository> {

    public static final String WHERE = "TicketSettingServiceImpl.findByUniqueFields: ";
    public static final String START_FIND = "Start " + WHERE + "{} , {}, {}";

    public List<TicketSettingUniqueFields> findByUniqueFields(TicketSetting ticketSetting) {
        log.trace(START_FIND, ticketSetting.getAccount(), ticketSetting.getGroup(), ticketSetting.getAuthor());
        return repository.findByUniqueFields(
                ticketSetting.getAccount().getId(),
                ticketSetting.getGroup() == null ? null : ticketSetting.getGroup().getId(),
                ticketSetting.getAuthor() == null ? null : ticketSetting.getAuthor().getId(),
                ticketSetting.getId()
        );
    }

}
