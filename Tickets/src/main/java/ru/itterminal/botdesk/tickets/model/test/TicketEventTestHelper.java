package ru.itterminal.botdesk.tickets.model.test;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.test.AccountTestHelper;
import ru.itterminal.botdesk.aau.model.test.GroupTestHelper;
import ru.itterminal.botdesk.aau.model.test.RoleTestHelper;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.commons.model.EntityTestHelperImpl;
import ru.itterminal.botdesk.tickets.model.TicketEvent;
import ru.itterminal.botdesk.tickets.model.dto.TicketEventDtoRequest;
import ru.itterminal.botdesk.tickets.model.dto.TicketEventDtoResponse;

@SuppressWarnings({"unused", "DuplicatedCode"})
public class TicketEventTestHelper extends EntityTestHelperImpl<TicketEvent, TicketEventDtoRequest, TicketEventDtoResponse> {

    private final AccountTestHelper accountTestHelper = new AccountTestHelper();
    private final GroupTestHelper groupTestHelper = new GroupTestHelper();
    private final UserTestHelper userTestHelper = new UserTestHelper();
    private final TicketTypeTestHelper ticketTypeTestHelper = new TicketTypeTestHelper();
    private final TicketStatusTestHelper ticketStatusTestHelper = new TicketStatusTestHelper();
    private final TicketTemplateTestHelper ticketTemplateTestHelper = new TicketTemplateTestHelper();
    private final RoleTestHelper roleTestHelper = new RoleTestHelper();
    private final TicketTestHelper ticketTestHelper = new TicketTestHelper();

    @Deprecated(since = "Must check before use!!!")
    @Override
    public TicketEvent getRandomValidEntity() {
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
        TicketEvent ticketEvent = TicketEvent.builder()
                .account(account)
                .newSubject(fakerRU.funnyName().toString())
                .newDescription(fakerRU.lorem().paragraph())
                .newAuthor(author)
                .createdAt(fakerRU.date().birthday().getTime())
                .createdBy(fakerRU.date().birthday().getTime())
                .newIsFinished(fakerRU.bool().bool())
                .newDeadline(null)
                .newObservers(null)
                .newExecutors(null)
                .newTicketType(ticketType)
                .newTicketStatus(ticketStatus)
                .ticketId(ticketTestHelper.getRandomValidEntity().getId())
                .build();
        setRandomValidPropertiesOfBaseEntity(ticketEvent);
        if (fakerRU.bool().bool()) {
            var executorOne = ticketEvent.getNewAuthor().toBuilder().build();
            executorOne.setRole(roleTestHelper.getRoleByName(Roles.EXECUTOR.toString()));
            var executorTwo = ticketEvent.getNewAuthor().toBuilder().build();
            executorTwo.setRole(roleTestHelper.getRoleByName(Roles.EXECUTOR.toString()));
            ticketEvent.setNewExecutors(List.of(executorOne, executorTwo));
        }
        if (fakerRU.bool().bool()) {
            var observerOne = ticketEvent.getNewAuthor().toBuilder().build();
            observerOne.setRole(roleTestHelper.getRoleByName(Roles.OBSERVER.toString()));
            var observerTwo = ticketEvent.getNewAuthor().toBuilder().build();
            observerTwo.setRole(roleTestHelper.getRoleByName(Roles.OBSERVER.toString()));
            ticketEvent.setNewObservers(List.of(observerOne, observerTwo));
        }
        return ticketEvent;
    }

    @Override
    public List<TicketEvent> setPredefinedValidEntityList() {
        return null;
    }

    @Override
    @SuppressWarnings("DuplicatedCode")
    public TicketEventDtoRequest convertEntityToDtoRequest(TicketEvent entity, boolean isDtoForCreate) {
        List<UUID> observersIdList = new ArrayList<>();
        if (entity.getNewObservers() != null) {
            observersIdList = entity.getNewObservers()
                    .stream()
                    .map(BaseEntity::getId)
                    .collect(Collectors.toList());
        }

        List<UUID> executorsIdList = new ArrayList<>();
        if (entity.getNewExecutors() != null) {
            executorsIdList = entity.getNewExecutors()
                    .stream()
                    .map(BaseEntity::getId)
                    .collect(Collectors.toList());
        }

        return TicketEventDtoRequest.builder()
                .id(entity.getId())
                .outId(entity.getOutId())
                .deleted(entity.getDeleted())
                .version(entity.getVersion())
                .displayName(entity.getDisplayName())
                .newAuthorId(entity.getNewAuthor() == null
                                ? null
                                : entity.getNewAuthor().getId())
                .newSubject(entity.getNewSubject())
                .newDescription(entity.getNewDescription())
                .newDeadline(entity.getNewDeadline())
                .newIsFinished(entity.getNewIsFinished())
                .newTicketTypeId(entity.getNewTicketType() == null
                                ? null
                                : entity.getNewTicketType().getId())
                .newTicketStatusId(entity.getNewTicketStatus() == null
                                    ? null
                                    : entity.getNewTicketStatus().getId())
                .newObservers(observersIdList)
                .newExecutors(executorsIdList)
                .build();
    }

}
