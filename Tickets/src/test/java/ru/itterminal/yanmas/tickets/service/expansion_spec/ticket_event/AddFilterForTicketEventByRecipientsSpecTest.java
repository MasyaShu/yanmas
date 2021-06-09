package ru.itterminal.yanmas.tickets.service.expansion_spec.ticket_event;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.tickets.model.TicketEvent;
import ru.itterminal.yanmas.tickets.model.dto.TicketEventFilterDto;
import ru.itterminal.yanmas.tickets.repository.TicketEventRepository;
import ru.itterminal.yanmas.tickets.repository.TicketRepositoryTestConfig;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;
import static ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {TicketRepositoryTestConfig.class, TicketEventRepository.class,
        AddFilterForTicketEventByRecipientsSpec.class, SpecificationsFactory.class})
@Sql({"/create-ticket-test.sql"})
class AddFilterForTicketEventByRecipientsSpecTest {

    @Autowired
    private TicketEventRepository ticketEventRepository;

    @Autowired
    private AddFilterForTicketEventByRecipientsSpec addFilterForTicketEventByRecipientsSpec;

    @Autowired
    private SpecificationsFactory specificationsFactory;

    private static final UUID ACCOUNT_1_ID = UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de");
    private static final UUID TICKED_1_ID = UUID.fromString("b927e5af-0db3-4f55-8da0-a31d0f6421d0");
    private static final UUID CURRENT_USER_ID = UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de");
    private final Pageable pageable = PageRequest.of(0, 5, Sort.by((Sort.Direction.ASC), "comment"));


    @Test
    void findAll_shouldFindOneRecordInDatabase_whenInDatabaseOnlyOne() {
        var currentUSer = User.builder()
                .id(CURRENT_USER_ID)
                .build();
        var ticketFilter = BaseEntityFilter.builder()
                .typeComparison(EXIST_IN.toString())
                .listOfIdEntities(List.of(TICKED_1_ID))
                .build();
        var filterDto = TicketEventFilterDto.builder()
                .ticket(ticketFilter)
                .build();
        var spec = specificationsFactory
                .makeSpecificationFromEntityFilterDto(TicketEvent.class, filterDto, ACCOUNT_1_ID);
        spec = addFilterForTicketEventByRecipientsSpec.expansionSpec(spec, currentUSer);
        var foundTicketEvent = ticketEventRepository.findAll(spec, pageable);
        assertEquals(3, foundTicketEvent.getContent().size());
    }

}