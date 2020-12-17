package ru.itterminal.botdesk.tickets.service.validator;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.projection.TicketStatusUniqueFields;
import ru.itterminal.botdesk.tickets.service.impl.TicketStatusServiceImpl;

import java.util.*;

import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@SpringJUnitConfig(value = {TicketStatusOperationValidator.class})
@ActiveProfiles("Test")
class TicketStatusOperationValidatorTest {
    @MockBean
    private TicketStatusServiceImpl service;

    @Mock
    private TicketStatusUniqueFields ticketStatusUniqueFields;

    @Autowired
    private final TicketStatusOperationValidator validator = new TicketStatusOperationValidator(service);

    private static final String EXIST_NAME = "ticketStatuss1";
    private static final Map<String, List<ValidationError>> errors = new HashMap<>();
    private static LogicalValidationException logicalValidationException;
    private static TicketStatus ticketStatus;

    @BeforeAll
    static void setUp() {
        Account account1 = new Account();
        account1.setId(UUID.fromString("bcf98101-2a22-42bf-94cc-c900b50a0b69"));
        ticketStatus = TicketStatus
                .builder()
                .name(EXIST_NAME)
                .account(account1)
                .build();
    }


    @Test
    void checkUniqueness_shouldGetTrue_whenPassedDataIsUnique() {
        when(service.findByUniqueFields(any())).thenReturn(Collections.emptyList());
        assertTrue(validator.checkUniqueness(new TicketStatus()));
    }

    @Test
    void checkUniqueness_shouldGetLogicalValidationException_whenPassedDataNotUnique() {
        when(service.findByUniqueFields(any())).thenReturn(List.of(ticketStatusUniqueFields));
        when(ticketStatusUniqueFields.getName()).thenReturn(EXIST_NAME);
        errors.put("name", singletonList(new ValidationError("not unique", "name is occupied")));
        logicalValidationException = new LogicalValidationException("Validation failed", errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.checkUniqueness(ticketStatus));
        assertEquals(logicalValidationException.getFieldErrors().get("name").get(0),
                thrown.getFieldErrors().get("name").get(0));
    }
    

}