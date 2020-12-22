package ru.itterminal.botdesk.tickets.model.test;

import ru.itterminal.botdesk.aau.model.test.AccountTestHelper;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.commons.model.EntityTestHelperImpl;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;
import ru.itterminal.botdesk.tickets.model.dto.TicketTemplateDtoRequest;
import ru.itterminal.botdesk.tickets.model.dto.TicketTemplateDtoResponse;

import java.util.ArrayList;
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
                .dateNextRun(1639144829000L)
                .dateStart(null)
                .dateEnd(null)
                .zoneId("Europe/Moscow")
                .expressionSchedule("25 6 5 25 2 *")
                .isOnlyOneTicketInWork(fakerRU.bool().bool())
                .isActive(fakerRU.bool().bool())
                .account(accountTestHelper.getRandomValidEntity())
                .Author(userTestHelper.getRandomValidEntity())
                .ticketType(ticketTypeTestHelper.getRandomValidEntity())
                .build();
        setRandomValidPropertiesOfBaseEntity(ticketTemplate);
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
}