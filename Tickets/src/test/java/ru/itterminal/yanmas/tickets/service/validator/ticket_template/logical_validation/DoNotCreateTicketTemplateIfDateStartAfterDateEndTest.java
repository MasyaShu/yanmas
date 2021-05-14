package ru.itterminal.yanmas.tickets.service.validator.ticket_template.logical_validation;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.tickets.model.test.TicketTemplateTestHelper;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;

@SpringJUnitConfig(value = {DoNotCreateTicketTemplateIfDateStartAfterDateEnd.class})
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class DoNotCreateTicketTemplateIfDateStartAfterDateEndTest {

    @Autowired
    private final DoNotCreateTicketTemplateIfDateStartAfterDateEnd validator = new DoNotCreateTicketTemplateIfDateStartAfterDateEnd();

    private final TicketTemplateTestHelper templateTestHelper = new TicketTemplateTestHelper();


    @Test
    void logicalValidationBeforeCreate_shouldAddErrorToMap_whenNameNotUnique() {
        var ticketTemplate = templateTestHelper.getRandomValidEntity();
        ticketTemplate.setDateStart(3L);
        ticketTemplate.setDateEnd(2L);
        var errors = createMapForLogicalErrors();
        assertEquals(0, errors.values().size());
        validator.logicalValidationBeforeCreate(ticketTemplate, errors);
        assertEquals(1, errors.values().size());
    }

    @Test
    void logicalValidationBeforeCreate_shouldNotAddErrorToMap_whenNameUnique() {
        var ticketTemplate = templateTestHelper.getRandomValidEntity();
        ticketTemplate.setDateStart(1L);
        ticketTemplate.setDateEnd(2L);
        var errors = createMapForLogicalErrors();
        assertEquals(0, errors.values().size());
        validator.logicalValidationBeforeCreate(ticketTemplate, errors);
        assertEquals(0, errors.values().size());
    }

}