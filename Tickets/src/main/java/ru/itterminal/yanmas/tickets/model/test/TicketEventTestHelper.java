package ru.itterminal.yanmas.tickets.model.test;

import ru.itterminal.yanmas.aau.model.test.UserTestHelper;
import ru.itterminal.yanmas.commons.model.EntityTestHelperImpl;
import ru.itterminal.yanmas.tickets.model.TicketEvent;
import ru.itterminal.yanmas.tickets.model.dto.TicketEventDtoRequest;
import ru.itterminal.yanmas.tickets.model.dto.TicketEventDtoResponse;

import java.util.List;

public class TicketEventTestHelper extends EntityTestHelperImpl<TicketEvent, TicketEventDtoRequest, TicketEventDtoResponse> {

    private final UserTestHelper userTestHelper = new UserTestHelper();
    private final TicketTestHelper ticketTestHelper = new TicketTestHelper();

    @Override
    public TicketEvent getRandomValidEntity() {
        var ticket = ticketTestHelper.getRandomValidEntity();
        var createdBy = userTestHelper.getRandomValidEntity();
        createdBy.setAccount(ticket.getAccount());
        ticket.setAuthor(createdBy);
        TicketEvent ticketEvent = TicketEvent.builder()
                .account(ticket.getAccount())
                .ticket(ticket)
                .comment(fakerRU.lorem().paragraph())
                .autoComment(fakerRU.lorem().paragraph())
                .createdBy(createdBy)
                .createdAt(System.currentTimeMillis())
                .isCommentForExecutors(false)
                .build();
        setRandomValidPropertiesOfBaseEntity(ticketEvent);
        return ticketEvent;
    }

    @Override
    public List<TicketEvent> setPredefinedValidEntityList() {
        return null;
    }

    @Override
    @SuppressWarnings("DuplicatedCode")
    public TicketEventDtoRequest convertEntityToDtoRequest(TicketEvent entity, boolean isDtoForCreate) {
        return TicketEventDtoRequest.builder()
                .id(entity.getId())
                .outId(entity.getOutId())
                .deleted(entity.getDeleted())
                .version(entity.getVersion())
                .displayName(entity.getDisplayName())
                .build();
    }

}
