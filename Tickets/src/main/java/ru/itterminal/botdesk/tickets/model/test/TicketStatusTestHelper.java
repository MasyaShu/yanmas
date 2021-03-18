package ru.itterminal.botdesk.tickets.model.test;

import org.modelmapper.ModelMapper;
import ru.itterminal.botdesk.aau.model.test.AccountTestHelper;
import ru.itterminal.botdesk.commons.model.EntityTestHelperImpl;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.dto.TicketStatusDto;

import java.util.List;
import java.util.UUID;

public class TicketStatusTestHelper extends EntityTestHelperImpl<TicketStatus, TicketStatusDto, TicketStatusDto> {

    private final AccountTestHelper accountTestHelper = new AccountTestHelper();
    private final ModelMapper modelMapper = new ModelMapper();

    @Override
    public TicketStatus getRandomValidEntity() {
        int isRandomPredefined = fakerRU.number().numberBetween(1, 6);
        TicketStatus ticketStatus = TicketStatus.builder()
                .name(fakerRU.hipster().word())
                .sortIndex(fakerRU.number().numberBetween(10, 101))
                .isStartedPredefined(isRandomPredefined == 1)
                .isFinishedPredefined(isRandomPredefined == 2)
                .isReopenedPredefined(isRandomPredefined == 3)
                .isCanceledPredefined(isRandomPredefined == 4)
                .account(accountTestHelper.getRandomValidEntity())
                .build();
        setRandomValidPropertiesOfBaseEntity(ticketStatus);
        return ticketStatus;
    }

    @Override
    public List<TicketStatus> setPredefinedValidEntityList() {
        TicketStatus ticketStatus1 = TicketStatus.builder()
                .name("started")
                .sortIndex(10)
                .isStartedPredefined(true)
                .isFinishedPredefined(false)
                .isReopenedPredefined(false)
                .isCanceledPredefined(false)
                .account(accountTestHelper.getPredefinedValidEntityList().get(0))
                .build();
        setPropertiesOfBaseEntity(ticketStatus1, UUID.fromString("7f66b241-f8ec-4912-8f58-a4ceef2dd4c9"),
                                  0, false, null);
        TicketStatus ticketStatus2 = TicketStatus.builder()
                .name("finished")
                .sortIndex(50)
                .isStartedPredefined(false)
                .isFinishedPredefined(true)
                .isReopenedPredefined(false)
                .isCanceledPredefined(false)
                .account(accountTestHelper.getPredefinedValidEntityList().get(0))
                .build();
        setPropertiesOfBaseEntity(ticketStatus2, UUID.fromString("17b13694-1907-4af9-8f5d-bfa444356e73"),
                                  0, false, null);
        TicketStatus ticketStatus3 = TicketStatus.builder()
                .name("reopened")
                .sortIndex(20)
                .isStartedPredefined(false)
                .isFinishedPredefined(false)
                .isReopenedPredefined(true)
                .isCanceledPredefined(false)
                .account(accountTestHelper.getPredefinedValidEntityList().get(1))
                .build();
        setPropertiesOfBaseEntity(ticketStatus3, UUID.fromString("dcf29ccb-26c7-4e38-9256-f45918a4c4a6"),
                                  0, false, null);
        TicketStatus ticketStatus4 = TicketStatus.builder()
                .name("canceled")
                .sortIndex(60)
                .isStartedPredefined(false)
                .isFinishedPredefined(false)
                .isReopenedPredefined(false)
                .isCanceledPredefined(true)
                .account(accountTestHelper.getPredefinedValidEntityList().get(1))
                .build();
        setPropertiesOfBaseEntity(ticketStatus4, UUID.fromString("e251aad2-1a47-46ed-a405-0d8a6468627e"),
                                  0, false, null);
        TicketStatus ticketStatus5 = TicketStatus.builder()
                .name("inWork")
                .sortIndex(40)
                .isStartedPredefined(false)
                .isFinishedPredefined(false)
                .isReopenedPredefined(false)
                .isCanceledPredefined(false)
                .account(accountTestHelper.getPredefinedValidEntityList().get(1))
                .build();
        setPropertiesOfBaseEntity(ticketStatus5, UUID.fromString("6dc9c0de-2143-40ce-ac65-5be97e3019fc"),
                                  0, false, null);
        return List.of(ticketStatus1, ticketStatus2, ticketStatus3, ticketStatus4, ticketStatus5);
    }

    @Override
    public TicketStatusDto convertEntityToDtoRequest(TicketStatus ticketStatus, boolean isDtoForCreate) {
        var ticketStatusDto = modelMapper.map(ticketStatus, TicketStatusDto.class);
        if (isDtoForCreate) {
            ticketStatusDto.setId(null);
            ticketStatusDto.setDeleted(null);
            ticketStatusDto.setVersion(null);
        }
        ticketStatusDto.setIsStartedPredefined(null);
        ticketStatusDto.setIsReopenedPredefined(null);
        ticketStatusDto.setIsCanceledPredefined(null);
        ticketStatusDto.setIsFinishedPredefined(null);
        ticketStatusDto.setDisplayName(null);
        return ticketStatusDto;
    }

}
