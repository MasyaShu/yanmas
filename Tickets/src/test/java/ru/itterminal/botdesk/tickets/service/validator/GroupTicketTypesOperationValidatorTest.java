package ru.itterminal.botdesk.tickets.service.validator;

import static java.lang.String.format;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.NOT_UNIQUE_CODE;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.NOT_UNIQUE_MESSAGE;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createLogicalValidationException;

import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.security.jwt.JwtUserBuilder;
import ru.itterminal.botdesk.tickets.model.GroupTicketTypes;
import ru.itterminal.botdesk.tickets.model.projection.GroupTicketTypesUniqueFields;
import ru.itterminal.botdesk.tickets.service.impl.GroupTicketTypesServiceImpl;

@SpringJUnitConfig(value = {GroupTicketTypesOperationValidator.class})
class GroupTicketTypesOperationValidatorTest {

    @Autowired
    private GroupTicketTypesOperationValidator validator;

    @MockBean
    private GroupTicketTypesServiceImpl service;

    @MockBean
    private JwtUserBuilder jwtUserBuilder;

    @MockBean
    GroupTicketTypesUniqueFields groupTicketTypesUniqueFields;

    private static final String EXIST_NAME = "Some existing name";
    private static final String NAME = "name";

    @Test
    void checkUniqueness_shouldGetTrue_whenPassedDataIsUnique() {
        when(service.findByUniqueFields(any())).thenReturn(Collections.emptyList());
        assertTrue(validator.checkUniqueness(new GroupTicketTypes()));
        verify(service, times(1)).findByUniqueFields(any());
    }

    @Test
    void checkUniqueness_shouldGetLogicalValidationException_whenPassedDataNotUnique() {
        when(service.findByUniqueFields(any())).thenReturn(List.of(groupTicketTypesUniqueFields));
        when(groupTicketTypesUniqueFields.getName()).thenReturn(EXIST_NAME);
        var expectedLogicalValidationException =
                createLogicalValidationException(NOT_UNIQUE_CODE, format(NOT_UNIQUE_MESSAGE, NAME));
        var groupTicketTypes = new GroupTicketTypes();
        var actualLogicalValidationException =
                assertThrows(LogicalValidationException.class, () -> validator.checkUniqueness(groupTicketTypes));
        assertEquals(
                expectedLogicalValidationException.getFieldErrors().get(NOT_UNIQUE_CODE).get(0),
                actualLogicalValidationException.getFieldErrors().get(NOT_UNIQUE_CODE).get(0)
        );
        verify(service, times(1)).findByUniqueFields(any());
    }
}
