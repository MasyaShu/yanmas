package ru.itterminal.yanmas.tickets.service.business_handler;

import org.springframework.scheduling.support.CronTrigger;
import org.springframework.scheduling.support.SimpleTriggerContext;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.tickets.model.TicketTemplate;

import java.time.ZoneId;
import java.util.Date;

@Component
public class TicketTemplateBusinessHandler implements EntityBusinessHandler<TicketTemplate> {

    @Override
    public void beforeCreate(TicketTemplate entity, User currentUser) {
        setNextExecutionTime(entity);
    }

    @Override
    public void beforeUpdate(TicketTemplate entity, User currentUser) {
        setNextExecutionTime(entity);
    }

    private void setNextExecutionTime(TicketTemplate ticketTemplate) {
        var cronTrigger = new CronTrigger(ticketTemplate.getExpressionSchedule(), ZoneId.of("GMT"));
        var triggerContext = new SimpleTriggerContext();
        var dateCountdown = System.currentTimeMillis();
        if (ticketTemplate.getDateStart() != null && ticketTemplate.getDateStart() >= System.currentTimeMillis()) {
            dateCountdown = ticketTemplate.getDateStart();
        }
        //noinspection ConstantConditions
        triggerContext.update(null, null, new Date(dateCountdown)); //NOSONAR
        Date dateNextRun;
        dateNextRun = cronTrigger.nextExecutionTime(triggerContext);
        if (dateNextRun != null && (ticketTemplate.getDateEnd() == null || ticketTemplate.getDateEnd() >= dateNextRun.getTime())) { //NOSONAR
            ticketTemplate.setDateNextRun(dateNextRun.getTime());
        }
    }
}
