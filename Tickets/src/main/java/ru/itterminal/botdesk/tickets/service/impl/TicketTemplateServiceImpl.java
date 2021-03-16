package ru.itterminal.botdesk.tickets.service.impl;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.support.CronTrigger;
import org.springframework.scheduling.support.SimpleTriggerContext;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.model.EntityConverter;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;
import ru.itterminal.botdesk.tickets.model.dto.TicketTemplateDtoRequest;
import ru.itterminal.botdesk.tickets.repository.TicketTemplateRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketTemplateOperationValidator;

import java.time.ZoneId;
import java.util.Date;
import java.util.UUID;

@SuppressWarnings("ConstantConditions")
@Slf4j
@Service
@Transactional
@RequiredArgsConstructor
public class TicketTemplateServiceImpl extends CrudServiceWithAccountImpl<TicketTemplate, TicketTemplateOperationValidator,
        TicketTemplateRepository> implements EntityConverter<TicketTemplate, TicketTemplateDtoRequest> {

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
        triggerContext.update(null, null, new Date(dateCountdown));
        Date dateNextRun;
        dateNextRun = cronTrigger.nextExecutionTime(triggerContext);
        if (dateNextRun != null && (ticketTemplate.getDateEnd() == null || ticketTemplate.getDateEnd() >= dateNextRun.getTime())) {
            ticketTemplate.setDateNextRun(dateNextRun.getTime());
        }
    }

    @Override
    public TicketTemplate convertRequestDtoIntoEntityWithNestedObjectsWithOnlyId(TicketTemplateDtoRequest request, UUID accountId) {
        var ticketTemplate = modelMapper.map(request, TicketTemplate.class);
        ticketTemplate.setAccount(Account.builder().id(accountId).build());
        return ticketTemplate;
    }

    @Override
    protected void setNestedObjectsOfEntityBeforeCreate(TicketTemplate entity) {
        super.setNestedObjectsOfEntityBeforeCreate(entity);
        entity.setDeleted(false);
        setNestedObjectsOfEntity(entity);
    }

    @Override
    protected void setNestedObjectsOfEntityBeforeUpdate(TicketTemplate entity) {
        super.setNestedObjectsOfEntityBeforeUpdate(entity);
        setNestedObjectsOfEntity(entity);
    }

    private void setNestedObjectsOfEntity(TicketTemplate entity) {
        entity.setAccount(accountService.findById(entity.getAccount().getId()));
        entity.setAuthor(userService.findByIdAndAccountId(entity.getAuthor().getId()));
        entity.setTicketType(ticketTypeService.findByIdAndAccountId(entity.getTicketType().getId()));
    }
}
