package ru.itterminal.botdesk.tickets.service.impl;

import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekObjectForNull;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.ifErrorsNotEmptyThrowLogicalValidationException;

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

    public static final String WHERE = "TicketSettingServiceImpl.findByUniqueFields: ";
    public static final String TICKET_SETTING_IS_NULL = WHERE + "TicketSetting is null";
    public static final String ACCOUNT_IS_NULL = WHERE + "Account is null";
    public static final String ACCOUNT = "Account is null";
    public static final String GROUP_IS_NULL = WHERE + "Group is null";
    public static final String GROUP = "Group is null";
    public static final String AUTHOR_IS_NULL = WHERE + "Author is null";
    public static final String AUTHOR = "Author is null";
    public static final String START_FIND = "Start " + WHERE + "{} , {}, {}";

    public List<TicketSettingUniqueFields> findByUniqueFields(TicketSetting ticketSetting) {
        chekObjectForNull(ticketSetting, TICKET_SETTING_IS_NULL, EntityNotExistException.class);
        var errors = createMapForLogicalErrors();
        chekObjectForNull(ticketSetting.getAccount(), ACCOUNT, ACCOUNT_IS_NULL, errors);
        chekObjectForNull(ticketSetting.getGroup(), GROUP, GROUP_IS_NULL, errors);
        chekObjectForNull(ticketSetting.getAuthor(), AUTHOR, AUTHOR_IS_NULL, errors);
        ifErrorsNotEmptyThrowLogicalValidationException(errors);
        log.trace(START_FIND, ticketSetting.getAccount(), ticketSetting.getGroup(), ticketSetting.getAuthor());
        return repository.findByUniqueFields(
                ticketSetting.getAccount().getId(),
                ticketSetting.getGroup().getId(),
                ticketSetting.getAuthor().getId(),
                ticketSetting.getId()
        );
    }

}
