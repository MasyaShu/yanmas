package ru.itterminal.yanmas.tickets.service.validator.ticket_status;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.security.config.TestSecurityConfig;
import ru.itterminal.yanmas.tickets.model.TicketStatus;
import ru.itterminal.yanmas.tickets.model.projection.TicketStatusUniqueFields;
import ru.itterminal.yanmas.tickets.model.test.TicketStatusTestHelper;
import ru.itterminal.yanmas.tickets.repository.TicketStatusRepository;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;

@SpringJUnitConfig(value = {LogicalValidationBeforeCreateCheckUniquesTicketStatus.class})
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class LogicalValidationBeforeCreateCheckUniquesTicketStatusTest {

    @MockBean
    private TicketStatusRepository repository;

    @MockBean
    private TicketStatusUniqueFields ticketStatusUniqueFields;

    @Autowired
    private final LogicalValidationBeforeCreateCheckUniquesTicketStatus validationBeforeCreateCheckUniquesTicketStatus =
            new LogicalValidationBeforeCreateCheckUniquesTicketStatus(repository);

    private final TicketStatusTestHelper ticketStatusTestHelper = new TicketStatusTestHelper();

    @Test
    void logicalValidationBeforeCreate_shouldAddErrorToMap_whenNameNotUnique() {
        TicketStatus ticketStatus = ticketStatusTestHelper.getRandomValidEntity();
        var errors = createMapForLogicalErrors();
        when(repository.getByNameAndAccount_Id(any(), any())).thenReturn(List.of(ticketStatusUniqueFields));
        assertEquals(0, errors.values().size());
        validationBeforeCreateCheckUniquesTicketStatus.logicalValidationBeforeCreate(ticketStatus, errors);
        assertEquals(1, errors.values().size());
        verify(repository, times(1)).getByNameAndAccount_Id(any(), any());
    }

    @Test
    void logicalValidationBeforeCreate_shouldNotAddErrorToMap_whenNameUnique() {
        TicketStatus ticketStatus = ticketStatusTestHelper.getRandomValidEntity();
        var errors = createMapForLogicalErrors();
        when(repository.getByNameAndAccount_Id(any(), any())).thenReturn(List.of());
        assertEquals(0, errors.values().size());
        validationBeforeCreateCheckUniquesTicketStatus.logicalValidationBeforeCreate(ticketStatus, errors);
        assertEquals(0, errors.values().size());
        verify(repository, times(1)).getByNameAndAccount_Id(any(), any());
    }
}