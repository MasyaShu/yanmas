package ru.itterminal.botdesk.tickets.model.test;

import java.util.List;
import java.util.UUID;

import ru.itterminal.botdesk.aau.model.test.AccountTestHelper;
import ru.itterminal.botdesk.commons.model.BaseTestEntityHelperImpl;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.dto.TicketTypeDto;
import ru.itterminal.botdesk.tickets.model.dto.TicketTypeFilterDto;

public class TicketTypeTestHelper extends BaseTestEntityHelperImpl<TicketType, TicketTypeDto, TicketTypeFilterDto> {

    private static final String INVALID_TICKET_TYPE_NAME_REGEX = "[A-Za-z0-9]{129}";
    private final AccountTestHelper accountTestHelper = new AccountTestHelper();

    @Override
    public TicketType getRandomValidEntity() {
        TicketType ticketType = TicketType.builder()
                .name(fakerRU.hipster().word())
                .comment(fakerRU.lorem().paragraph())
                .isPredefined(fakerRU.bool().bool())
                .account(accountTestHelper.getRandomValidEntity())
                .build();
        setRandomValidPropertiesOfBaseEntity(ticketType);
        return ticketType;
    }

    @Override
    public TicketType getRandomInvalidEntity() {
        TicketType ticketType = TicketType.builder()
                .name(fakerRU.regexify(INVALID_TICKET_TYPE_NAME_REGEX))
                .comment(fakerRU.lorem().paragraph())
                .isPredefined(fakerRU.bool().bool())
                .account(accountTestHelper.getRandomValidEntity())
                .build();
        setRandomValidPropertiesOfBaseEntity(ticketType);
        return ticketType;
    }

    @Override
    public List<TicketType> setPredefinedValidEntityList() {
        TicketType ticketType1 = TicketType.builder()
                .name("ticketTypes1")
                .comment("comment1")
                .isPredefined(true)
                .account(accountTestHelper.getPredefinedValidEntityList().get(0))
                .build();
        setPropertiesOfBaseEntity(ticketType1, UUID.fromString("7f66b241-f8ec-4912-8f58-a4ceef2dd4c9"),
                                  0, false, null);
        TicketType ticketType2 = TicketType.builder()
                .name("ticketTypes2")
                .comment("comment2")
                .isPredefined(false)
                .account(accountTestHelper.getPredefinedValidEntityList().get(0))
                .build();
        setPropertiesOfBaseEntity(ticketType2, UUID.fromString("17b13694-1907-4af9-8f5d-bfa444356e73"),
                                  0, false, null);
        TicketType ticketType3 = TicketType.builder()
                .name("ticketTypes3")
                .comment("comment3")
                .isPredefined(true)
                .account(accountTestHelper.getPredefinedValidEntityList().get(1))
                .build();
        setPropertiesOfBaseEntity(ticketType3, UUID.fromString("dcf29ccb-26c7-4e38-9256-f45918a4c4a6"),
                                  0, false, null);
        TicketType ticketType4 = TicketType.builder()
                .name("ticketTypes4")
                .comment("comment4")
                .isPredefined(false)
                .account(accountTestHelper.getPredefinedValidEntityList().get(1))
                .build();
        setPropertiesOfBaseEntity(ticketType4, UUID.fromString("e251aad2-1a47-46ed-a405-0d8a6468627e"),
                                  0, false, null);
        TicketType ticketType5 = TicketType.builder()
                .name("ticketTypes5")
                .comment("")
                .isPredefined(false)
                .account(accountTestHelper.getPredefinedValidEntityList().get(1))
                .build();
        setPropertiesOfBaseEntity(ticketType5, UUID.fromString("6dc9c0de-2143-40ce-ac65-5be97e3019fc"),
                                  0, false, null);
        return List.of(ticketType1, ticketType2, ticketType3, ticketType4, ticketType5);
    }
}