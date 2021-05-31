package ru.itterminal.yanmas.tickets.model.test;

import ru.itterminal.yanmas.aau.model.Account;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.test.AccountTestHelper;
import ru.itterminal.yanmas.aau.model.test.GroupTestHelper;
import ru.itterminal.yanmas.aau.model.test.RoleTestHelper;
import ru.itterminal.yanmas.aau.model.test.UserTestHelper;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.commons.model.EntityTestHelperImpl;
import ru.itterminal.yanmas.commons.model.dto.BaseEntityDto;
import ru.itterminal.yanmas.commons.model.filter.*;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.model.dto.TicketDtoRequest;
import ru.itterminal.yanmas.tickets.model.dto.TicketDtoResponse;
import ru.itterminal.yanmas.tickets.model.dto.TicketFilterDto;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import static ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;
import static ru.itterminal.yanmas.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.CONTAINS_ALL_OF_LIST;
import static ru.itterminal.yanmas.commons.model.filter.NumberFilter.TypeComparisonForNumberFilter.IS_EQUAL_TO;
import static ru.itterminal.yanmas.commons.model.filter.StringFilter.TypeComparisonForStringFilter.TEXT_EQUALS;

public class TicketTestHelper extends EntityTestHelperImpl<Ticket, TicketDtoRequest, TicketDtoResponse> {

    private final AccountTestHelper accountTestHelper = new AccountTestHelper();
    private final GroupTestHelper groupTestHelper = new GroupTestHelper();
    private final UserTestHelper userTestHelper = new UserTestHelper();
    private final TicketTypeTestHelper ticketTypeTestHelper = new TicketTypeTestHelper();
    private final TicketStatusTestHelper ticketStatusTestHelper = new TicketStatusTestHelper();
    private final TicketTemplateTestHelper ticketTemplateTestHelper = new TicketTemplateTestHelper();
    private final RoleTestHelper roleTestHelper = new RoleTestHelper();

    @SuppressWarnings("DuplicatedCode")
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
        if (fakerRU.bool().bool()) {
            var executorOne = ticket.getAuthor().toBuilder().build();
            executorOne.setRole(roleTestHelper.getRoleByName(Roles.EXECUTOR.toString()));
            var executorTwo = ticket.getAuthor().toBuilder().build();
            executorTwo.setRole(roleTestHelper.getRoleByName(Roles.EXECUTOR.toString()));
            ticket.setExecutors(List.of(executorOne, executorTwo));
        }
        if (fakerRU.bool().bool()) {
            var observerOne = ticket.getAuthor().toBuilder().build();
            observerOne.setRole(roleTestHelper.getRoleByName(Roles.OBSERVER.toString()));
            var observerTwo = ticket.getAuthor().toBuilder().build();
            observerTwo.setRole(roleTestHelper.getRoleByName(Roles.OBSERVER.toString()));
            ticket.setObservers(List.of(observerOne, observerTwo));
        }
        return ticket;
    }

    @SuppressWarnings({"DuplicatedCode", "unused"})
    public Ticket getRandomValidEntityWithAccount(Account account) {
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
        if (fakerRU.bool().bool()) {
            var executorOne = ticket.getAuthor().toBuilder().build();
            executorOne.setRole(roleTestHelper.getRoleByName(Roles.EXECUTOR.toString()));
            var executorTwo = ticket.getAuthor().toBuilder().build();
            executorTwo.setRole(roleTestHelper.getRoleByName(Roles.EXECUTOR.toString()));
            ticket.setExecutors(List.of(executorOne, executorTwo));
        }
        if (fakerRU.bool().bool()) {
            var observerOne = ticket.getAuthor().toBuilder().build();
            observerOne.setRole(roleTestHelper.getRoleByName(Roles.OBSERVER.toString()));
            var observerTwo = ticket.getAuthor().toBuilder().build();
            observerTwo.setRole(roleTestHelper.getRoleByName(Roles.OBSERVER.toString()));
            ticket.setObservers(List.of(observerOne, observerTwo));
        }
        return ticket;
    }

    @Override
    public List<Ticket> setPredefinedValidEntityList() {
        return null; //NOSONAR
    }

    @Override
    @SuppressWarnings("DuplicatedCode")
    public TicketDtoRequest convertEntityToDtoRequest(Ticket entity, boolean isDtoForCreate) {
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
                .authorId(entity.getAuthor() == null
                                ? null
                                : entity.getAuthor().getId())
                .subject(entity.getSubject())
                .description(entity.getDescription())
                .deadline(entity.getDeadline())
                .isFinished(entity.getIsFinished())
                .ticketTypeId(entity.getTicketType() == null
                                ? null
                                : entity.getTicketType().getId())
                .ticketStatusId(entity.getTicketStatus() == null
                                    ? null
                                    : entity.getTicketStatus().getId())
                .observers(observersIdList)
                .executors(executorsIdList)
                .files(filesIdList)
                .priority(entity.getPriority())
                .build();
    }

    public TicketFilterDto convertTicketDtoResponseToTicketFilterDto(TicketDtoResponse ticket) {
        var filterDto = TicketFilterDto.builder()
                .author(BaseEntityFilter.builder()
                        .typeComparison(EXIST_IN.toString())
                        .listOfIdEntities(List.of(ticket.getAuthor().getId()))
                        .build())
                .group(BaseEntityFilter.builder()
                        .typeComparison(EXIST_IN.toString())
                        .listOfIdEntities(List.of(ticket.getGroup().getId()))
                        .build())
                .number(NumberFilter.builder()
                        .typeComparison(IS_EQUAL_TO.toString())
                        .valueOne(ticket.getNumber())
                        .build())
                .createdAt(NumberFilter.builder()
                        .typeComparison(IS_EQUAL_TO.toString())
                        .valueOne(ticket.getCreatedAt())
                        .build())
                .isFinished(BooleanFilter.builder()
                        .value(ticket.getIsFinished())
                        .build())
                .ticketType(BaseEntityFilter.builder()
                        .typeComparison(EXIST_IN.toString())
                        .listOfIdEntities(List.of(ticket.getTicketType().getId()))
                        .build())
                .ticketStatus(BaseEntityFilter.builder()
                        .typeComparison(EXIST_IN.toString())
                        .listOfIdEntities(List.of(ticket.getTicketStatus().getId()))
                        .build())
                .deleted(BooleanFilter.builder()
                        .value(ticket.getDeleted())
                        .build())
                .build();

        if (ticket.getOutId() != null && !ticket.getOutId().isEmpty()) {
            filterDto.setOutId(StringFilter.builder()
                    .value(ticket.getOutId())
                    .typeComparison(TEXT_EQUALS.toString())
                    .build());
        }
        if (ticket.getSubject() != null && !ticket.getSubject().isEmpty()) {
            filterDto.setSubject(StringFilter.builder()
                    .value(ticket.getSubject())
                    .typeComparison(TEXT_EQUALS.toString())
                    .build());
        }
        if (ticket.getDescription() != null && !ticket.getDescription().isEmpty()) {
            filterDto.setDescription(StringFilter.builder()
                    .value(ticket.getDescription())
                    .typeComparison(TEXT_EQUALS.toString())
                    .build());
        }
        if (ticket.getDeadline() != null) {
            filterDto.setDeadline(NumberFilter.builder()
                    .typeComparison(IS_EQUAL_TO.toString())
                    .valueOne(ticket.getDeadline())
                    .build());
        }
        if (ticket.getTicketTemplate() != null) {
            filterDto.setTicketTemplate(BaseEntityFilter.builder()
                    .typeComparison(EXIST_IN.toString())
                    .listOfIdEntities(List.of(ticket.getTicketTemplate().getId()))
                    .build());
        }

        if (ticket.getObservers() != null && !ticket.getObservers().isEmpty()) {
            var observersIdList = ticket.getObservers()
                    .stream()
                    .map(BaseEntityDto::getId)
                    .collect(Collectors.toList());
            var observers = ListOfBaseEntityFilter.builder()
                    .typeComparison(CONTAINS_ALL_OF_LIST.toString())
                    .listOfIdEntities(observersIdList)
                    .build();
            filterDto.setObservers(observers);
        }

        if (ticket.getExecutors() != null && !ticket.getExecutors().isEmpty()) {
            var executorsIdList = ticket.getExecutors()
                    .stream()
                    .map(BaseEntityDto::getId)
                    .collect(Collectors.toList());
            var executors = ListOfBaseEntityFilter.builder()
                    .typeComparison(CONTAINS_ALL_OF_LIST.toString())
                    .listOfIdEntities(executorsIdList)
                    .build();
            filterDto.setExecutors(executors);
        }

        if (ticket.getFiles() != null && !ticket.getFiles().isEmpty()) {
            var filesIdList = ticket.getFiles()
                    .stream()
                    .map(BaseEntityDto::getId)
                    .collect(Collectors.toList());
            var files = ListOfBaseEntityFilter.builder()
                    .typeComparison(CONTAINS_ALL_OF_LIST.toString())
                    .listOfIdEntities(filesIdList)
                    .build();
            filterDto.setFiles(files);
        }

        return filterDto;
    }

}
