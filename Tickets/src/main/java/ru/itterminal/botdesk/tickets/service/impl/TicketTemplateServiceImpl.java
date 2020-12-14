package ru.itterminal.botdesk.tickets.service.impl;


import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.support.CronTrigger;
import org.springframework.scheduling.support.SimpleTriggerContext;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceImpl;
import ru.itterminal.botdesk.commons.util.CommonMethodsForValidation;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;
import ru.itterminal.botdesk.tickets.repository.TicketTemplateRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketTemplateOperationValidator;

import java.time.ZoneId;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.util.Collections.singletonList;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.LOGIC_CONSTRAINT_CODE;

@Slf4j
@Service
@Transactional
public class TicketTemplateServiceImpl extends CrudServiceImpl<TicketTemplate, TicketTemplateOperationValidator, TicketTemplateRepository> {

    public static final String ERROR_NEXT_RUN = "error in determining the time of the next execution";

    @Override
    public TicketTemplate create(TicketTemplate entity) {
        nextExecutionTime(entity);
        return super.create(entity);
    }

    @Override
    public TicketTemplate update(TicketTemplate entity) {
        nextExecutionTime(entity);
        return super.update(entity);
    }

    private void nextExecutionTime(TicketTemplate ticketTemplate) {
        var lastScheduledExecutionTime = (ticketTemplate.getDateStart() == null)? null: new Date(ticketTemplate.getDateStart());;
        var lastActualExecutionTime = new Date(System.currentTimeMillis());
        var lastCompletionTime =  (ticketTemplate.getDateEnd() == null)? null: new Date(ticketTemplate.getDateEnd());
        CronTrigger cronTrigger = new CronTrigger(ticketTemplate.getExpressionSchedule(), ZoneId.of(ticketTemplate.getZoneId()));
        SimpleTriggerContext triggerContext = new SimpleTriggerContext(lastScheduledExecutionTime, lastActualExecutionTime, lastCompletionTime);
        Date dateNextRun = null;
        try {
            dateNextRun = cronTrigger.nextExecutionTime(triggerContext);
            assert dateNextRun != null;
            ticketTemplate.setDateNextRun(dateNextRun.getTime());
        } catch (IllegalArgumentException ex) {
            CommonMethodsForValidation.createExpectedLogicalValidationException(ERROR_NEXT_RUN, ex.getMessage());
        }
    }
}
