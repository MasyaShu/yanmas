package ru.itterminal.botdesk.tickets.service.impl;

import java.time.ZoneId;
import java.util.Date;

import org.springframework.scheduling.support.CronTrigger;
import org.springframework.scheduling.support.SimpleTriggerContext;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;
import ru.itterminal.botdesk.tickets.repository.TicketTemplateRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketTemplateOperationValidator;

@SuppressWarnings("ConstantConditions")
@Slf4j
@Service
@Transactional
@RequiredArgsConstructor
public class TicketTemplateServiceImpl extends CrudServiceWithAccountImpl<TicketTemplate, TicketTemplateOperationValidator,
        TicketTemplateRepository> {

    private final AccountServiceImpl accountService;
    private final TicketTypeServiceImpl ticketTypeService;
    private final UserServiceImpl userService;

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
        triggerContext.update(null, null, new Date(dateCountdown)); //NOSONAR
        Date dateNextRun;
        dateNextRun = cronTrigger.nextExecutionTime(triggerContext);
        if (dateNextRun != null && (ticketTemplate.getDateEnd() == null || ticketTemplate.getDateEnd() >= dateNextRun.getTime())) { //NOSONAR
            ticketTemplate.setDateNextRun(dateNextRun.getTime());
        }
    }

    @Override
    protected void setNestedObjectsOfEntityBeforeCreate(TicketTemplate entity) {
        setNestedObjectsOfEntity(entity);
    }

    @Override
    protected void setNestedObjectsOfEntityBeforeUpdate(TicketTemplate entity) {
        setNestedObjectsOfEntity(entity);
    }

    private void setNestedObjectsOfEntity(TicketTemplate entity) {
        entity.setAccount(accountService.findById(entity.getAccount().getId()));
        entity.setAuthor(userService.findByIdAndAccountId(entity.getAuthor().getId()));
        entity.setTicketType(ticketTypeService.findByIdAndAccountId(entity.getTicketType().getId()));
    }
}
