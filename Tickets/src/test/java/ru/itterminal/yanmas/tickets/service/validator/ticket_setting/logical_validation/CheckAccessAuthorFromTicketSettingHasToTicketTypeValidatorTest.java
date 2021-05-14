package ru.itterminal.yanmas.tickets.service.validator.ticket_setting.logical_validation;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.tickets.model.TicketSetting;
import ru.itterminal.yanmas.tickets.model.test.TicketSettingTestHelper;
import ru.itterminal.yanmas.tickets.service.impl.SettingsAccessToTicketTypesServiceImpl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;

@SpringJUnitConfig(value = {CheckAccessAuthorFromTicketSettingHasToTicketTypeValidator.class})
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class CheckAccessAuthorFromTicketSettingHasToTicketTypeValidatorTest {

    @Autowired
    private CheckAccessAuthorFromTicketSettingHasToTicketTypeValidator checkAccessAuthorFromTicketSettingHasToTicketTypeValidator;

    @MockBean
    private SettingsAccessToTicketTypesServiceImpl settingsAccessToTicketTypesService;

    private final TicketSettingTestHelper ticketSettingTestHelper = new TicketSettingTestHelper();


    @Test
    void checkAccessAuthorFromTicketSettingHasToTicketType_shouldNoError_whenAccessIsAvailable() {
        TicketSetting ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        var errors = createMapForLogicalErrors();
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(true);
        assertEquals(0, errors.values().size());
        checkAccessAuthorFromTicketSettingHasToTicketTypeValidator.logicalValidationBeforeUpdate(ticketSetting, errors);
        assertEquals(0, errors.values().size());
    }

    @Test
    void checkAccessAuthorFromTicketSettingHasToTicketType_shouldError_whenAccessDenied() {
        TicketSetting ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        var errors = createMapForLogicalErrors();
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(false);
        assertEquals(0, errors.values().size());
        checkAccessAuthorFromTicketSettingHasToTicketTypeValidator.logicalValidationBeforeUpdate(ticketSetting, errors);
        assertEquals(1, errors.values().size());
    }

}