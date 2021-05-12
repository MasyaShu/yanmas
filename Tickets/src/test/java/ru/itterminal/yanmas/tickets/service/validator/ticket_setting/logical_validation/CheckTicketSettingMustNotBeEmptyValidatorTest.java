package ru.itterminal.yanmas.tickets.service.validator.ticket_setting.logical_validation;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.tickets.model.TicketSetting;
import ru.itterminal.yanmas.tickets.model.test.TicketSettingTestHelper;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;

@SpringJUnitConfig(value = {CheckTicketSettingMustNotBeEmptyValidator.class})
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class CheckTicketSettingMustNotBeEmptyValidatorTest {

    @Autowired
    private CheckTicketSettingMustNotBeEmptyValidator ticketSettingMustNotBeEmptyValidator;

    private final TicketSettingTestHelper ticketSettingTestHelper = new TicketSettingTestHelper();

    @Test
    void beforeUpdate_shouldGetTrue_whenPassedPredefinedValidEntity() {
        TicketSetting ticketSetting = ticketSettingTestHelper.getRandomValidEntity();
        var errors = createMapForLogicalErrors();
        assertEquals(0, errors.values().size());
        ticketSettingMustNotBeEmptyValidator.logicalValidationBeforeUpdate(ticketSetting, errors);
        assertEquals(0, errors.values().size());
    }

    @Test
    void beforeUpdate_shouldGetLogicalValidationException_whenAllPassedSettingIsNull() {
        TicketSetting ticketSetting = new TicketSetting();
        var errors = createMapForLogicalErrors();
        assertEquals(0, errors.values().size());
        ticketSettingMustNotBeEmptyValidator.logicalValidationBeforeUpdate(ticketSetting, errors);
        assertEquals(1, errors.values().size());
    }

}