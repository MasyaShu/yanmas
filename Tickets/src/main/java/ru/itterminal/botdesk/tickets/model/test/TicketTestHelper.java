package ru.itterminal.botdesk.tickets.model.test;

import java.util.List;

import ru.itterminal.botdesk.aau.model.test.AccountTestHelper;
import ru.itterminal.botdesk.aau.model.test.GroupTestHelper;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
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

    @Override
    public Ticket getRandomValidEntity() {
        var account  = accountTestHelper.getRandomValidEntity();
        var group = groupTestHelper.getRandomValidEntity();
        group.setAccount(account);
        var author = userTestHelper.getRandomValidEntity();
        author.setAccount(account);
        author.setGroup(group);
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

}
