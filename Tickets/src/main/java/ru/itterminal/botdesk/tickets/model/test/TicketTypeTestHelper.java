package ru.itterminal.botdesk.tickets.model.test;

import org.modelmapper.ModelMapper;
import ru.itterminal.botdesk.aau.model.test.AccountTestHelper;
import ru.itterminal.botdesk.commons.model.EntityTestHelperImpl;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.dto.TicketTypeDto;

import java.util.List;
import java.util.UUID;

public class TicketTypeTestHelper extends EntityTestHelperImpl<TicketType, TicketTypeDto, TicketTypeDto> {

    private final AccountTestHelper accountTestHelper = new AccountTestHelper();
    private final ModelMapper modelMapper = new ModelMapper();

    @Override
    public TicketType getRandomValidEntity() {
        TicketType ticketType = TicketType.builder()
                .name(fakerRU.name().firstName())
                .comment(fakerRU.lorem().paragraph())
                .isPredefinedForNewTicket(fakerRU.bool().bool())
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
                .isPredefinedForNewTicket(true)
                .account(accountTestHelper.getPredefinedValidEntityList().get(0))
                .build();
        setPropertiesOfBaseEntity(ticketType1, UUID.fromString("7f66b241-f8ec-4912-8f58-a4ceef2dd4c9"),
                                  0, false, null);
        TicketType ticketType2 = TicketType.builder()
                .name("ticketTypes2")
                .comment("comment2")
                .isPredefinedForNewTicket(false)
                .account(accountTestHelper.getPredefinedValidEntityList().get(0))
                .build();
        setPropertiesOfBaseEntity(ticketType2, UUID.fromString("17b13694-1907-4af9-8f5d-bfa444356e73"),
                                  0, false, null);
        TicketType ticketType3 = TicketType.builder()
                .name("ticketTypes3")
                .comment("comment3")
                .isPredefinedForNewTicket(true)
                .account(accountTestHelper.getPredefinedValidEntityList().get(1))
                .build();
        setPropertiesOfBaseEntity(ticketType3, UUID.fromString("dcf29ccb-26c7-4e38-9256-f45918a4c4a6"),
                                  0, false, null);
        TicketType ticketType4 = TicketType.builder()
                .name("ticketTypes4")
                .comment("comment4")
                .isPredefinedForNewTicket(false)
                .account(accountTestHelper.getPredefinedValidEntityList().get(1))
                .build();
        setPropertiesOfBaseEntity(ticketType4, UUID.fromString("e251aad2-1a47-46ed-a405-0d8a6468627e"),
                                  0, false, null);
        TicketType ticketType5 = TicketType.builder()
                .name("ticketTypes5")
                .comment("")
                .isPredefinedForNewTicket(false)
                .account(accountTestHelper.getPredefinedValidEntityList().get(1))
                .build();
        setPropertiesOfBaseEntity(ticketType5, UUID.fromString("6dc9c0de-2143-40ce-ac65-5be97e3019fc"),
                                  0, false, null);
        return List.of(ticketType1, ticketType2, ticketType3, ticketType4, ticketType5);
    }

    @Override
    public TicketTypeDto convertEntityToDtoRequest(TicketType ticketType, boolean isDtoForCreate) {
        var ticketTypeDto = modelMapper.map(ticketType, TicketTypeDto.class);
        if (isDtoForCreate) {
            ticketTypeDto.setId(null);
            ticketTypeDto.setDeleted(null);
            ticketTypeDto.setVersion(null);
        }
        ticketTypeDto.setIsPredefinedForNewTicket(null);
        ticketTypeDto.setDisplayName(null);
        return ticketTypeDto;
    }
}
