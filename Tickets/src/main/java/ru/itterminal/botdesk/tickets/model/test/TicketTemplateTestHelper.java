package ru.itterminal.botdesk.tickets.model.test;

import ru.itterminal.botdesk.aau.model.test.AccountTestHelper;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.commons.model.EntityTestHelperImpl;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;
import ru.itterminal.botdesk.tickets.model.dto.TicketTemplateDtoRequest;
import ru.itterminal.botdesk.tickets.model.dto.TicketTemplateDtoResponse;

import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.UUID;

public class TicketTemplateTestHelper extends EntityTestHelperImpl<TicketTemplate, TicketTemplateDtoRequest,
        TicketTemplateDtoResponse> {

    private final AccountTestHelper accountTestHelper = new AccountTestHelper();
    private final UserTestHelper userTestHelper = new UserTestHelper();
    private final TicketTypeTestHelper ticketTypeTestHelper = new TicketTypeTestHelper();

    @Override
    public TicketTemplate getRandomValidEntity() {
        TicketTemplate ticketTemplate = TicketTemplate.builder()
                .subject(fakerRU.hipster().word())
                .description(fakerRU.lorem().paragraph())
                .dateNextRun(null)
                .dateStart(null)
                .dateEnd(null)
                .zoneId("Europe/Moscow")
                .expressionSchedule("25 6 5 25 2 *")
                .isOnlyOneTicketInWork(fakerRU.bool().bool())
                .isActive(fakerRU.bool().bool())
                .account(accountTestHelper.getRandomValidEntity())
                .Author(userTestHelper.getRandomValidEntity())
                .ticketType(ticketTypeTestHelper.getRandomValidEntity())
                .id(UUID.randomUUID())
                .version(fakerRU.number().numberBetween(0, 100))
                .deleted(1 == fakerRU.number().numberBetween(0, 2))
                .outId(UUID.randomUUID().toString())
                .build();
        ticketTemplate.generateDisplayName();
        return ticketTemplate;
    }

    @Override
    public List<TicketTemplate> setPredefinedValidEntityList() {
        List<TicketTemplate> ticketTemplates = new ArrayList<>();
        TicketTemplate ticketTemplate1 = TicketTemplate.builder()
                .subject("subject_1")
                .description("description_1")
                .dateNextRun(1639144829000L)
                .dateStart(null)
                .dateEnd(null)
                .zoneId("Europe/Moscow")
                .expressionSchedule("25 6 5 25 2 *")
                .isOnlyOneTicketInWork(true)
                .isActive(true)
                .account(accountTestHelper
                        .getEntityFromPredefinedValidEntityByEntityId("cdfa6483-0769-4628-ba32-efd338a716de"))
                .Author(userTestHelper
                        .getEntityFromPredefinedValidEntityByEntityId("d592facb-e6ee-4801-8310-9c7708eb6e6c"))
                .ticketType(ticketTypeTestHelper
                        .getEntityFromPredefinedValidEntityByEntityId("7f66b241-f8ec-4912-8f58-a4ceef2dd4c9"))
                .build();
        setPropertiesOfBaseEntity(ticketTemplate1, UUID.fromString("21dad366-54d8-445f-b778-4cc3829e07b1"),
                0, false, null
        );
        ticketTemplates.add(ticketTemplate1);
        TicketTemplate ticketTemplate2 = TicketTemplate.builder()
                .subject("subject_2")
                .description("description_2")
                .dateNextRun(1639144829000L)
                .dateStart(null)
                .dateEnd(null)
                .zoneId("America/New_York")
                .expressionSchedule("25 6 5 25 2,4,7 *")
                .isOnlyOneTicketInWork(false)
                .isActive(true)
                .account(accountTestHelper
                        .getEntityFromPredefinedValidEntityByEntityId("cdfa6483-0769-4628-ba32-efd338a716de"))
                .Author(userTestHelper
                        .getEntityFromPredefinedValidEntityByEntityId("d592facb-e6ee-4801-8310-9c7708eb6e6c"))
                .ticketType(ticketTypeTestHelper
                        .getEntityFromPredefinedValidEntityByEntityId("17b13694-1907-4af9-8f5d-bfa444356e73"))
                .build();
        setPropertiesOfBaseEntity(ticketTemplate1, UUID.fromString("f8a773d2-0f4d-48e9-b788-7ce671373992"),
                0, false, null
        );
        ticketTemplates.add(ticketTemplate2);
        return ticketTemplates;
    }

    @Override
    public TicketTemplateDtoRequest convertEntityToDtoRequest(TicketTemplate entity) {
        return TicketTemplateDtoRequest.builder()
                .dateEnd(entity.getDateEnd())
                .dateStart(entity.getDateStart())
                .description(entity.getDescription())
                .expressionSchedule(entity.getExpressionSchedule())
                .isActive(entity.getIsActive())
                .subject(entity.getSubject())
                .zoneId(entity.getZoneId())
                .id(entity.getId())
                .version(entity.getVersion())
                .deleted(entity.getDeleted())
                .isOnlyOneTicketInWork(entity.getIsOnlyOneTicketInWork())
                .outId(entity.getOutId())
                .authorId(entity.getAuthor() == null ? null : entity.getAuthor().getId())
                .ticketTypeId(entity.getTicketType() == null ? null : entity.getTicketType().getId())
                .build();
    }


    public static Date atStartOfDay(Date date) {
        LocalDateTime localDateTime = dateToLocalDateTime(date);
        LocalDateTime startOfDay = localDateTime.with(LocalTime.MIN);
        return localDateTimeToDate(startOfDay);
    }

    public static Date atEndOfDay(Date date) {
        LocalDateTime localDateTime = dateToLocalDateTime(date);
        LocalDateTime endOfDay = localDateTime.with(LocalTime.MAX);
        return localDateTimeToDate(endOfDay);
    }

    private static LocalDateTime dateToLocalDateTime(Date date) {
        return LocalDateTime.ofInstant(date.toInstant(), ZoneId.of("GMT"));
    }

    private static Date localDateTimeToDate(LocalDateTime localDateTime) {
        return Date.from(localDateTime.atZone(ZoneId.of("GMT")).toInstant());
    }
}
