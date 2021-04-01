package ru.itterminal.yanmas.tickets.service.validator;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Import;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.commons.exception.LogicalValidationException;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.security.config.TestSecurityConfig;
import ru.itterminal.yanmas.tickets.model.TicketTemplate;
import ru.itterminal.yanmas.tickets.model.test.TicketTemplateTestHelper;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.*;
import static ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl.VALIDATION_FAILED;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;


@SpringJUnitConfig(value = {TicketTemplateOperationValidator.class})
@Import(TestSecurityConfig.class)
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class TicketTemplateOperationValidatorTest {

    @Autowired
    private TicketTemplateOperationValidator validator;

    private static final Map<String, List<ValidationError>> errors = new HashMap<>();
    private final TicketTemplateTestHelper templateTestHelper = new TicketTemplateTestHelper();

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void checkDateStartAfterDateEnd_shouldGetTrue_whenDateStartAndDateEndNull() {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        assertTrue(validator.logicalValidationBeforeCreate(ticketTemplate));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void checkDateStartAfterDateEnd_shouldGetTrue_whenDateStartBeforeDateEndNull() {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        ticketTemplate.setDateStart(System.currentTimeMillis());
        ticketTemplate.setDateEnd(ticketTemplate.getDateStart() + 860000L);
        assertTrue(validator.logicalValidationBeforeCreate(ticketTemplate));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void checkDateStartAfterDateEnd_shouldGetLogicalValidationException_whenDateStartAfterDateEndNull() {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        ticketTemplate.setDateStart(System.currentTimeMillis());
        ticketTemplate.setDateEnd(ticketTemplate.getDateStart() - 860000L);
        errors.put(VALIDATION_FAILED, singletonList(new ValidationError(VALIDATION_FAILED, TicketTemplateOperationValidator.THAN_DATE_END)));
        LogicalValidationException logicalValidationException = new LogicalValidationException(VALIDATION_FAILED, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.logicalValidationBeforeCreate(ticketTemplate));
        assertEquals(logicalValidationException.getFieldErrors().get(VALIDATION_FAILED).get(0),
                thrown.getFieldErrors().get(VALIDATION_FAILED).get(0));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeCreate_shouldGetAccessDeniedException_whenCurrentUserFromNotInnerGroup() {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        AccessDeniedException thrown = assertThrows(AccessDeniedException.class,
                () -> validator.checkAccessBeforeCreate(ticketTemplate));
        Assertions.assertEquals(TicketTemplateOperationValidator.A_USER_FROM_NOT_INNER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_TEMPLATE,
                thrown.getMessage());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeUpdate_shouldGetAccessDeniedException_whenCurrentUserFromNotInnerGroup() {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        AccessDeniedException thrown = assertThrows(AccessDeniedException.class,
                () -> validator.checkAccessBeforeUpdate(ticketTemplate));
        Assertions.assertEquals(TicketTemplateOperationValidator.A_USER_FROM_NOT_INNER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_TEMPLATE,
                thrown.getMessage());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetTrue_whenCurrentUserFromInnerGroup() {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        assertTrue(validator.logicalValidationBeforeCreate(ticketTemplate));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetTrue_whenCurrentUserFromInnerGroup() {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        assertTrue(validator.logicalValidationBeforeUpdate(ticketTemplate));
    }
}
