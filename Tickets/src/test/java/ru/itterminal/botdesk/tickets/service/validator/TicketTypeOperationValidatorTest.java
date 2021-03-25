package ru.itterminal.botdesk.tickets.service.validator;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.projection.TicketTypeUniqueFields;
import ru.itterminal.botdesk.tickets.service.impl.TicketTypeServiceImpl;

import java.util.*;

import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static ru.itterminal.botdesk.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;
import static ru.itterminal.botdesk.tickets.service.validator.TicketTypeOperationValidator.USER_FROM_OUTER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_TYPE;

@SpringJUnitConfig(value = {TicketTypeOperationValidator.class})
@Import(TestSecurityConfig.class)
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class TicketTypeOperationValidatorTest {

    @MockBean
    private TicketTypeServiceImpl service;

    @MockBean
    private TicketTypeUniqueFields ticketTypeUniqueFields;

    @Autowired
    private final TicketTypeOperationValidator validator = new TicketTypeOperationValidator(service);

    private static final String EXIST_NAME = "ticketTypes1";
    private static final String VALIDATED_FIELDS = "name";
    private static final Map<String, List<ValidationError>> errors = new HashMap<>();
    private static TicketType ticketType;

    @BeforeAll
    static void setUp() {
        Account account1 = new Account();
        account1.setId(UUID.fromString("bcf98101-2a22-42bf-94cc-c900b50a0b69"));
        ticketType = TicketType
                .builder()
                .name(EXIST_NAME)
                .account(account1)
                .build();
    }


    @Test
    void checkUniqueness_shouldGetTrue_whenPassedDataIsUnique() {
        when(service.findByUniqueFields(any())).thenReturn(Collections.emptyList());
        assertTrue(validator.checkUniqueness(new TicketType()));
    }

    @Test
    void checkUniqueness_shouldGetLogicalValidationException_whenPassedDataNotUnique() {
        when(service.findByUniqueFields(any())).thenReturn(List.of(ticketTypeUniqueFields));
        when(ticketTypeUniqueFields.getName()).thenReturn(EXIST_NAME);
        errors.put(VALIDATED_FIELDS, singletonList(new ValidationError(VALIDATED_FIELDS, "name is occupied")));
        LogicalValidationException logicalValidationException = new LogicalValidationException(VALIDATED_FIELDS, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.checkUniqueness(ticketType));
        assertEquals(logicalValidationException.getFieldErrors().get("name").get(0),
                thrown.getFieldErrors().get("name").get(0));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeCreate_shouldGetAccessDeniedException_whenCurrentUserFromNotInnerGroup() {
        AccessDeniedException thrown = assertThrows(AccessDeniedException.class,
                () -> validator.checkAccessBeforeCreate(ticketType));
        assertEquals(
                USER_FROM_OUTER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_TYPE,
                thrown.getMessage());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeUpdate_shouldGetAccessDeniedException_whenCurrentUserFromNotInnerGroup() {
        AccessDeniedException thrown = assertThrows(AccessDeniedException.class,
                () -> validator.checkAccessBeforeUpdate(ticketType));
        assertEquals(
                USER_FROM_OUTER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_TYPE,
                thrown.getMessage());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetTrue_whenCurrentUserFromInnerGroup() {
        assertTrue(validator.beforeCreate(ticketType));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetTrue_whenCurrentUserFromInnerGroup() {
        assertTrue(validator.beforeUpdate(ticketType));
    }
}
