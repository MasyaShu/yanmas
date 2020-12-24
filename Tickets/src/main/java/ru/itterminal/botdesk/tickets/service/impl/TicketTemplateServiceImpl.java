package ru.itterminal.botdesk.tickets.service.impl;


import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.support.CronTrigger;
import org.springframework.scheduling.support.SimpleTriggerContext;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceImpl;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;
import ru.itterminal.botdesk.tickets.repository.TicketTemplateRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketTemplateOperationValidator;

import java.time.ZoneId;
import java.util.Date;

@SuppressWarnings("ConstantConditions")
@Slf4j
@Service
@Transactional
public class TicketTemplateServiceImpl extends CrudServiceWithAccountImpl<TicketTemplate, TicketTemplateOperationValidator, TicketTemplateRepository> {

    @Override
    public TicketTemplate create(TicketTemplate entity) {
        setNextExecutionTime(entity);
        return super.create(entity);
    }

    @Override
    public TicketTemplate update(TicketTemplate entity) {
        setNextExecutionTime(entity);
        return super.update(entity);
    }

    private void setNextExecutionTime(TicketTemplate ticketTemplate) {
        CronTrigger cronTrigger = new CronTrigger(ticketTemplate.getExpressionSchedule(), ZoneId.of("GMT"));
        SimpleTriggerContext triggerContext = new SimpleTriggerContext();
        var dateCountdown = System.currentTimeMillis();
        if (ticketTemplate.getDateStart() != null && ticketTemplate.getDateStart() >= System.currentTimeMillis()) {
            dateCountdown = ticketTemplate.getDateStart();
        }
        //noinspection ConstantConditions
        triggerContext.update(null, null, new Date(dateCountdown));
        Date dateNextRun;
        dateNextRun = cronTrigger.nextExecutionTime(triggerContext);
        if (dateNextRun != null && (ticketTemplate.getDateEnd() == null || ticketTemplate.getDateEnd() >= dateNextRun.getTime())) {
            ticketTemplate.setDateNextRun(dateNextRun.getTime());
        }
    }
}
