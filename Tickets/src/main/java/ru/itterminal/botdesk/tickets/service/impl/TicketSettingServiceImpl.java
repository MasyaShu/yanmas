package ru.itterminal.botdesk.tickets.service.impl;

import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekObjectForNull;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceImpl;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.projection.TicketSettingUniqueFields;
import ru.itterminal.botdesk.tickets.repository.TicketSettingRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketSettingOperationValidator;

@Slf4j
@Service
@Transactional
public class TicketSettingServiceImpl extends CrudServiceImpl<TicketSetting, TicketSettingOperationValidator,
        TicketSettingRepository> {

    private static final String WHERE = "TicketSettingServiceImpl.findByUniqueFields: ";
    private static final String TICKET_SETTING_IS_NULL = WHERE + "TicketSetting is null";
    private static final String ACCOUNT_IS_NULL = WHERE + "Account is null";
    private static final String GROUP_IS_NULL = WHERE + "Group is null";
    private static final String AUTHOR_IS_NULL = WHERE + "Author is null";
    private static final String START_FIND = "Start " + WHERE + "{} , {}, {}";

    public List<TicketSettingUniqueFields> findByUniqueFields(TicketSetting ticketSetting) {
        chekObjectForNull(ticketSetting, TICKET_SETTING_IS_NULL, EntityNotExistException.class);
        chekObjectForNull(ticketSetting.getAccount(), ACCOUNT_IS_NULL, EntityNotExistException.class);
        chekObjectForNull(ticketSetting.getAccount(), GROUP_IS_NULL, EntityNotExistException.class);
        chekObjectForNull(ticketSetting.getAccount(), AUTHOR_IS_NULL, EntityNotExistException.class);
        log.trace(START_FIND, ticketSetting.getAccount(), ticketSetting.getGroup(), ticketSetting.getAuthor());
        return repository.findByUniqueFields(
                ticketSetting.getAccount().getId(),
                ticketSetting.getGroup().getId(),
                ticketSetting.getAuthor().getId(),
                ticketSetting.getId()
        );
    }

}
