package ru.itterminal.botdesk.tickets.model.test;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import ru.itterminal.botdesk.aau.model.test.AccountTestHelper;
import ru.itterminal.botdesk.aau.model.test.GroupTestHelper;
import ru.itterminal.botdesk.aau.model.test.RoleTestHelper;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.commons.model.EntityTestHelperImpl;
import ru.itterminal.botdesk.tickets.model.Ticket;
import ru.itterminal.botdesk.tickets.model.dto.TicketDtoRequest;
import ru.itterminal.botdesk.tickets.model.dto.TicketDtoResponse;

public class TicketTestHelper extends EntityTestHelperImpl<Ticket, TicketDtoRequest, TicketDtoResponse> {

    private final AccountTestHelper accountTestHelper = new AccountTestHelper();
    private final GroupTestHelper groupTestHelper = new GroupTestHelper();
    private final UserTestHelper userTestHelper = new UserTestHelper();
    private final TicketTypeTestHelper ticketTypeTestHelper = new TicketTypeTestHelper();
    private final TicketStatusTestHelper ticketStatusTestHelper = new TicketStatusTestHelper();
    private final TicketTemplateTestHelper ticketTemplateTestHelper = new TicketTemplateTestHelper();
    private final RoleTestHelper roleTestHelper = new RoleTestHelper();

    @Override
    public Ticket getRandomValidEntity() {
        var account = accountTestHelper.getRandomValidEntity();
        var group = groupTestHelper.getRandomValidEntity();
        group.setAccount(account);
        var author = userTestHelper.getRandomValidEntity();
        author.setAccount(account);
        author.setGroup(group);
        author.setRole(roleTestHelper.getPredefinedValidEntityList().get(3));
        var ticketType = ticketTypeTestHelper.getRandomValidEntity();
        ticketType.setAccount(account);
        var ticketStatus = ticketStatusTestHelper.getRandomValidEntity();
        ticketStatus.setAccount(account);
        var ticketTemplate = ticketTemplateTestHelper.getRandomValidEntity();
        ticketTemplate.setAccount(account);
        Ticket ticket = Ticket.builder()
                .account(account)
                .number(fakerRU.number().randomNumber())
                .subject(fakerRU.funnyName().toString())
                .description(fakerRU.lorem().paragraph())
                .author(author)
                .createdAt(fakerRU.date().birthday().getTime())
                .isFinished(fakerRU.bool().bool())
                .deadline(null)
                .group(group)
                .observers(null)
                .executors(null)
                .files(null)
                .ticketType(ticketType)
                .ticketStatus(ticketStatus)
                .ticketTemplate(ticketTemplate)
                .build();
        setRandomValidPropertiesOfBaseEntity(ticket);

        return ticket;
    }

    @Override
    public List<Ticket> setPredefinedValidEntityList() {
        return null;
    }

    @Override
    @SuppressWarnings("DuplicatedCode")
    public TicketDtoRequest convertEntityToDtoRequest(Ticket entity) {
        List<UUID> observersIdList = new ArrayList<>();
        if (entity.getObservers() != null) {
            observersIdList = entity.getObservers()
                    .stream()
                    .map(BaseEntity::getId)
                    .collect(Collectors.toList());
        }

        List<UUID> executorsIdList = new ArrayList<>();
        if (entity.getExecutors() != null) {
            executorsIdList = entity.getExecutors()
                    .stream()
                    .map(BaseEntity::getId)
                    .collect(Collectors.toList());
        }

        List<UUID> filesIdList = new ArrayList<>();
        if (entity.getFiles() != null) {
            filesIdList = entity.getFiles()
                    .stream()
                    .map(BaseEntity::getId)
                    .collect(Collectors.toList());
        }

        return TicketDtoRequest.builder()
                .id(entity.getId())
                .outId(entity.getOutId())
                .deleted(entity.getDeleted())
                .version(entity.getVersion())
                .displayName(entity.getDisplayName())
                .author(entity.getAuthor() == null
                                ? null
                                : entity.getAuthor().getId())
                .subject(entity.getSubject())
                .description(entity.getDescription())
                .deadline(entity.getDeadline())
                .isFinished(entity.getIsFinished())
                .ticketType(entity.getTicketType() == null
                                ? null
                                : entity.getTicketType().getId())
                .ticketStatus(entity.getTicketStatus() == null
                                    ? null
                                    : entity.getTicketStatus().getId())
                .ticketTemplate(entity.getTicketTemplate() == null
                                      ? null
                                      : entity.getTicketTemplate().getId())
                .observers(observersIdList)
                .executors(executorsIdList)
                .files(filesIdList)
                .build();
    }

}
