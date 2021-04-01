package ru.itterminal.yanmas.tickets.model.test;

import org.modelmapper.ModelMapper;
import ru.itterminal.yanmas.aau.model.test.AccountTestHelper;
import ru.itterminal.yanmas.aau.model.test.UserTestHelper;
import ru.itterminal.yanmas.commons.model.EntityTestHelperImpl;
import ru.itterminal.yanmas.tickets.model.TicketTemplate;
import ru.itterminal.yanmas.tickets.model.dto.TicketTemplateDtoRequest;
import ru.itterminal.yanmas.tickets.model.dto.TicketTemplateDtoResponse;

import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.UUID;

public class TicketTemplateTestHelper extends EntityTestHelperImpl<TicketTemplate, TicketTemplateDtoRequest,
        TicketTemplateDtoResponse> {

    public static final String EUROPE_MOSCOW = "Europe/Moscow";
    private static final String ACCOUNT_ID_1 = "cdfa6483-0769-4628-ba32-efd338a716de";
    public static final String AUTHOR_ID_1 = "d592facb-e6ee-4801-8310-9c7708eb6e6c";
    public static final String AUTHOR_ID_2 = "86840939-c488-448b-a473-cd9e1097dd32";
    public static final String TICKET_TYPE_ID_1 = "7f66b241-f8ec-4912-8f58-a4ceef2dd4c9";
    private static final String ACCOUNT_ID_2 = "bcf98101-2a22-42bf-94cc-c900b50a0b69";
    public static final String TICKET_TYPE_ID_2 = "dcf29ccb-26c7-4e38-9256-f45918a4c4a6";
    private static final String EXPRESSION_SCHEDULE_1 = "25 6 5 25 2 *";
    private static final String EXPRESSION_SCHEDULE_2 = "25 6 5 25 2,4,7 *";
    private static final String AMERICA_NEW_YORK = "America/New_York";
    public static final long DATE_2019_01_01 = 1546300800000L;
    public static final long DATE_2020_01_01 = 1577836800000L;
    public static final long DATE_2021_01_01 = 1609459200000L;
    public static final long DATE_2024_01_01 = 1704067200000L;
    public static final long DATE_2099_01_01 = 4070908800000L;
    public static final String TICKET_TYPE_ID_3 = "17b13694-1907-4af9-8f5d-bfa444356e73";
    private final AccountTestHelper accountTestHelper = new AccountTestHelper();
    private final UserTestHelper userTestHelper = new UserTestHelper();
    private final TicketTypeTestHelper ticketTypeTestHelper = new TicketTypeTestHelper();
    private final ModelMapper modelMapper = new ModelMapper();

    @Override
    public TicketTemplate getRandomValidEntity() {
        TicketTemplate ticketTemplate = TicketTemplate.builder()
                .subject(fakerRU.hipster().word())
                .description(fakerRU.lorem().paragraph())
                .dateNextRun(null)
                .dateStart(null)
                .dateEnd(null)
                .zoneId(getValidZoneId())
                .expressionSchedule(EXPRESSION_SCHEDULE_1)
                .isOnlyOneTicketInWork(fakerRU.bool().bool())
                .isActive(fakerRU.bool().bool())
                .account(accountTestHelper.getRandomValidEntity())
                .author(userTestHelper.getRandomValidEntity())
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
                .dateStart(DATE_2019_01_01)
                .dateEnd(DATE_2021_01_01)
                .zoneId(EUROPE_MOSCOW)
                .expressionSchedule(EXPRESSION_SCHEDULE_1)
                .isOnlyOneTicketInWork(true)
                .isActive(true)
                .account(accountTestHelper
                        .getEntityFromPredefinedValidEntityByEntityId(ACCOUNT_ID_1))
                .author(userTestHelper
                        .getEntityFromPredefinedValidEntityByEntityId(AUTHOR_ID_1))
                .ticketType(ticketTypeTestHelper
                        .getEntityFromPredefinedValidEntityByEntityId(TICKET_TYPE_ID_1))
                .outId(null)
                .deleted(false)
                .version(0)
                .id(UUID.fromString("21dad366-54d8-445f-b778-4cc3829e07b1"))
                .build();
        ticketTemplate1.generateDisplayName();
        ticketTemplates.add(ticketTemplate1);

        TicketTemplate ticketTemplate2 = TicketTemplate.builder()
                .subject("subject_2")
                .description("description_2")
                .dateNextRun(1639144829000L)
                .dateStart(DATE_2020_01_01)
                .dateEnd(DATE_2024_01_01)
                .zoneId(AMERICA_NEW_YORK)
                .expressionSchedule(EXPRESSION_SCHEDULE_2)
                .isOnlyOneTicketInWork(false)
                .isActive(true)
                .account(accountTestHelper
                        .getEntityFromPredefinedValidEntityByEntityId(ACCOUNT_ID_1))
                .author(userTestHelper
                        .getEntityFromPredefinedValidEntityByEntityId(AUTHOR_ID_1))
                .ticketType(ticketTypeTestHelper
                        .getEntityFromPredefinedValidEntityByEntityId(TICKET_TYPE_ID_3))
                .outId(null)
                .deleted(false)
                .version(0)
                .id(UUID.fromString("f8a773d2-0f4d-48e9-b788-7ce671373992"))
                .build();
        ticketTemplate2.generateDisplayName();
        ticketTemplates.add(ticketTemplate2);

        TicketTemplate ticketTemplate3 = TicketTemplate.builder()
                .subject("subject_3")
                .description("description_3")
                .dateNextRun(1639144829000L)
                .dateStart(DATE_2019_01_01)
                .dateEnd(DATE_2099_01_01)
                .zoneId(EUROPE_MOSCOW)
                .expressionSchedule(EXPRESSION_SCHEDULE_1)
                .isOnlyOneTicketInWork(true)
                .isActive(true)
                .account(accountTestHelper
                        .getEntityFromPredefinedValidEntityByEntityId(ACCOUNT_ID_1))
                .author(userTestHelper
                        .getEntityFromPredefinedValidEntityByEntityId(AUTHOR_ID_1))
                .ticketType(ticketTypeTestHelper
                        .getEntityFromPredefinedValidEntityByEntityId(TICKET_TYPE_ID_1))
                .outId(null)
                .deleted(false)
                .version(0)
                .id(UUID.fromString("8525adcb-9edd-4af5-aa66-a211f47465f8"))
                .build();
        ticketTemplate3.generateDisplayName();
        ticketTemplates.add(ticketTemplate3);

        TicketTemplate ticketTemplate4 = TicketTemplate.builder()
                .subject("subject_4")
                .description("description_4")
                .dateNextRun(1639144829000L)
                .dateStart(DATE_2020_01_01)
                .dateEnd(null)
                .zoneId(AMERICA_NEW_YORK)
                .expressionSchedule(EXPRESSION_SCHEDULE_2)
                .isOnlyOneTicketInWork(false)
                .isActive(false)
                .account(accountTestHelper
                        .getEntityFromPredefinedValidEntityByEntityId(ACCOUNT_ID_2))
                .author(userTestHelper
                        .getEntityFromPredefinedValidEntityByEntityId(AUTHOR_ID_2))
                .ticketType(ticketTypeTestHelper
                        .getEntityFromPredefinedValidEntityByEntityId(TICKET_TYPE_ID_2))
                .outId(null)
                .deleted(false)
                .version(0)
                .id(UUID.fromString("4713d994-18fc-4629-aa95-9792bbc53215"))
                .build();
        ticketTemplate4.generateDisplayName();
        ticketTemplates.add(ticketTemplate4);

        TicketTemplate ticketTemplate5 = TicketTemplate.builder()
                .subject("subject_5")
                .description(null)
                .dateNextRun(1639144829000L)
                .dateStart(null)
                .dateEnd(null)
                .zoneId(EUROPE_MOSCOW)
                .expressionSchedule(EXPRESSION_SCHEDULE_1)
                .isOnlyOneTicketInWork(true)
                .isActive(false)
                .account(accountTestHelper
                        .getEntityFromPredefinedValidEntityByEntityId(ACCOUNT_ID_2))
                .author(userTestHelper
                        .getEntityFromPredefinedValidEntityByEntityId(AUTHOR_ID_2))
                .ticketType(null)
                .outId(null)
                .deleted(false)
                .version(0)
                .id(UUID.fromString("bf052d6c-b9ed-479a-b04b-0fa083c371c9"))
                .build();
        ticketTemplate5.generateDisplayName();
        ticketTemplates.add(ticketTemplate5);
        return ticketTemplates;
    }

    @Override
    public TicketTemplateDtoRequest convertEntityToDtoRequest(TicketTemplate ticketTemplate, boolean isDtoForCreate) {
        var ticketTemplateDtoRequest = modelMapper.map(ticketTemplate, TicketTemplateDtoRequest.class);
        if (isDtoForCreate) {
            ticketTemplateDtoRequest.setId(null);
            ticketTemplateDtoRequest.setDeleted(null);
            ticketTemplateDtoRequest.setVersion(null);
        }
        ticketTemplateDtoRequest.setDisplayName(null);
        return ticketTemplateDtoRequest;
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

    private String getValidZoneId() {
        var setZoneId = ZoneId.getAvailableZoneIds();
        var index = fakerRU.number().numberBetween(0, setZoneId.size() - 1);
        return setZoneId.toArray()[index].toString();
    }
}
