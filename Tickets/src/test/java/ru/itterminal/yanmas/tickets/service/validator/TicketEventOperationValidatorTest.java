package ru.itterminal.yanmas.tickets.service.validator;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.provider.Arguments;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.tickets.model.TicketEvent;
import ru.itterminal.yanmas.tickets.model.test.TicketEventTestHelper;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.yanmas.tickets.service.validator.TicketEventOperationValidator.EMPTY_FIELDS;
import static ru.itterminal.yanmas.tickets.service.validator.TicketEventOperationValidator.MUST_NOT_CREATE_EVENT_IF_FIELDS_COMMENT_AUTO_COMMENT_AND_FILES_ARE_EMPTY;

@SpringJUnitConfig(value = {TicketEventOperationValidator.class})
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class TicketEventOperationValidatorTest {

    @MockBean
    private TicketOperationValidator ticketOperationValidator;

    @Autowired
    private TicketEventOperationValidator validator;

    private final TicketEventTestHelper ticketEventTestHelper = new TicketEventTestHelper();

    @Test
    void isEmptyCommentAutoCommentAndFiles_shouldAddErrorToMap_whenFieldsIsEmpty() {
        var errors = createMapForLogicalErrors();
        var ticketEvent = TicketEvent.builder().build();
        validator.isEmptyCommentAutoCommentAndFiles(ticketEvent, errors);
        assertEquals(MUST_NOT_CREATE_EVENT_IF_FIELDS_COMMENT_AUTO_COMMENT_AND_FILES_ARE_EMPTY, errors.get(EMPTY_FIELDS).get(0).getMessage());
        assertFalse(errors.isEmpty());
    }

    @Test
    void isEmptyCommentAutoCommentAndFiles_shouldNotAddErrorToMap_whenCommentIsNotEmpty() {
        var errors = createMapForLogicalErrors();
        var ticketEvent = ticketEventTestHelper.getRandomValidEntity();
        validator.isEmptyCommentAutoCommentAndFiles(ticketEvent, errors);
        assertTrue(errors.isEmpty());
    }

    private static Stream<Arguments> getTicketEvent() {
        return Stream.of(

        );
    }

}