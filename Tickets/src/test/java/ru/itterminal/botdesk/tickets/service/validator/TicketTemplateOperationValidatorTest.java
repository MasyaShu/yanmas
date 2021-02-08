package ru.itterminal.botdesk.tickets.service.validator;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Import;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;
import ru.itterminal.botdesk.tickets.model.test.TicketTemplateTestHelper;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.*;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.VALIDATION_FAILED;
import static ru.itterminal.botdesk.tickets.service.validator.TicketTemplateOperationValidator.A_USER_FROM_NOT_INNER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_TEMPLATE;
import static ru.itterminal.botdesk.tickets.service.validator.TicketTemplateOperationValidator.THAN_DATE_END;


@SpringJUnitConfig(value = {TicketTemplateOperationValidator.class})
@Import(TestSecurityConfig.class)
@ActiveProfiles("Test")
class TicketTemplateOperationValidatorTest {

    @Autowired
    private TicketTemplateOperationValidator validator;

    private static final Map<String, List<ValidationError>> errors = new HashMap<>();
    private final TicketTemplateTestHelper templateTestHelper = new TicketTemplateTestHelper();

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void checkDateStartAfterDateEnd_shouldGetTrue_whenDateStartAndDateEndNull() {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        assertTrue(validator.beforeCreate(ticketTemplate));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void checkDateStartAfterDateEnd_shouldGetTrue_whenDateStartBeforeDateEndNull() {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        ticketTemplate.setDateStart(System.currentTimeMillis());
        ticketTemplate.setDateEnd(ticketTemplate.getDateStart() + 860000L);
        assertTrue(validator.beforeCreate(ticketTemplate));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void checkDateStartAfterDateEnd_shouldGetLogicalValidationException_whenDateStartAfterDateEndNull() {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        ticketTemplate.setDateStart(System.currentTimeMillis());
        ticketTemplate.setDateEnd(ticketTemplate.getDateStart() - 860000L);
        errors.put(VALIDATION_FAILED, singletonList(new ValidationError(VALIDATION_FAILED, THAN_DATE_END)));
        LogicalValidationException logicalValidationException = new LogicalValidationException(VALIDATION_FAILED, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.beforeCreate(ticketTemplate));
        assertEquals(logicalValidationException.getFieldErrors().get(VALIDATION_FAILED).get(0),
                thrown.getFieldErrors().get(VALIDATION_FAILED).get(0));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeCreate_shouldGetAccessDeniedException_whenCurrentUserFromNotInnerGroup() {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        AccessDeniedException thrown = assertThrows(AccessDeniedException.class,
                () -> validator.beforeCreate(ticketTemplate));
        assertEquals(A_USER_FROM_NOT_INNER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_TEMPLATE,
                thrown.getMessage());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeUpdate_shouldGetAccessDeniedException_whenCurrentUserFromNotInnerGroup() {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        AccessDeniedException thrown = assertThrows(AccessDeniedException.class,
                () -> validator.beforeUpdate(ticketTemplate));
        assertEquals(A_USER_FROM_NOT_INNER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_TEMPLATE,
                thrown.getMessage());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetTrue_whenCurrentUserFromInnerGroup() {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        assertTrue(validator.beforeCreate(ticketTemplate));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetTrue_whenCurrentUserFromInnerGroup() {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        assertTrue(validator.beforeUpdate(ticketTemplate));
    }
}