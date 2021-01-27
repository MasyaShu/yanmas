package ru.itterminal.botdesk.tickets.service.validator;

import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.VALIDATION_FAILED;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.EMPTY_TICKET;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.MUST_NOT_CREATE_UPDATE_TICKET_IF_SUBJECT_DESCRIPTION_AND_FILES_ARE_EMPTY;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.tickets.model.Ticket;

@SpringJUnitConfig(value = {TicketOperationValidator.class})
class TicketOperationValidatorTest {

    @Autowired
    private final TicketOperationValidator validator = new TicketOperationValidator();

    @Test
    void beforeCreate_shouldGetLogicalValidationException_whenTicketIsNew () {
        var expectedErrors = createMapForLogicalErrors();
        expectedErrors.put(
                EMPTY_TICKET,
                singletonList(
                        new ValidationError(
                                EMPTY_TICKET,
                                MUST_NOT_CREATE_UPDATE_TICKET_IF_SUBJECT_DESCRIPTION_AND_FILES_ARE_EMPTY
                        )
                )
        );
        LogicalValidationException expectedException =
                new LogicalValidationException(VALIDATION_FAILED, expectedErrors);
        var ticket = new Ticket();
        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.beforeCreate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(EMPTY_TICKET).get(0),
                actualException.getFieldErrors().get(EMPTY_TICKET).get(0)
        );
    }

    @Test
    void beforeCreate_shouldGetTrue_whenPassedTicketIsValid () {
        var ticket = Ticket.builder()
                .subject("some subject")
                .build();
        var actualResult = validator.beforeCreate(ticket);
        assertTrue(actualResult);
    }

    @Test
    void beforeUpdate_shouldGetLogicalValidationException_whenTicketIsNew () {
        var expectedErrors = createMapForLogicalErrors();
        expectedErrors.put(
                EMPTY_TICKET,
                singletonList(
                        new ValidationError(
                                EMPTY_TICKET,
                                MUST_NOT_CREATE_UPDATE_TICKET_IF_SUBJECT_DESCRIPTION_AND_FILES_ARE_EMPTY
                        )
                )
        );
        LogicalValidationException expectedException =
                new LogicalValidationException(VALIDATION_FAILED, expectedErrors);
        var ticket = new Ticket();
        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.beforeUpdate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(EMPTY_TICKET).get(0),
                actualException.getFieldErrors().get(EMPTY_TICKET).get(0)
        );
    }

    @Test
    void beforeUpdate_shouldGetTrue_whenPassedTicketIsValid () {
        var ticket = Ticket.builder()
                .subject("some subject")
                .build();
        var actualResult = validator.beforeUpdate(ticket);
        assertTrue(actualResult);
    }

}