package ru.itterminal.yanmas.tickets.service.validator;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.aau.model.Account;
import ru.itterminal.yanmas.commons.exception.LogicalValidationException;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;
import ru.itterminal.yanmas.tickets.model.GroupTicketTypes;
import ru.itterminal.yanmas.tickets.model.projection.GroupTicketTypesUniqueFields;
import ru.itterminal.yanmas.tickets.repository.GroupTicketTypesRepository;

import java.util.Collections;
import java.util.List;

import static java.lang.String.format;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl.NOT_UNIQUE_CODE;
import static ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl.NOT_UNIQUE_MESSAGE;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createLogicalValidationException;

@SpringJUnitConfig(value = {GroupTicketTypesOperationValidator.class})
class GroupTicketTypesOperationValidatorTest {

    @Autowired
    private GroupTicketTypesOperationValidator validator;

    @MockBean
    private GroupTicketTypesRepository repository;

    @MockBean
    private JwtUserBuilder jwtUserBuilder;

    @MockBean
    GroupTicketTypesUniqueFields groupTicketTypesUniqueFields;

    private static final String EXIST_NAME = "Some existing name";
    private static final String NAME = "name";

    @Test
    void checkUniqueness_shouldGetTrue_whenPassedDataIsUnique() {
        when(repository.getByNameAndAccount_IdAndIdNot(any(), any(), any())).thenReturn(Collections.emptyList());
        assertTrue(validator.checkUniqueness(GroupTicketTypes.builder().account(new Account()).build()));
        verify(repository, times(1)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }

    @Test
    void checkUniqueness_shouldGetLogicalValidationException_whenPassedDataNotUnique() {
        when(repository.getByNameAndAccount_IdAndIdNot(any(), any(), any())).thenReturn(List.of(groupTicketTypesUniqueFields));
        when(groupTicketTypesUniqueFields.getName()).thenReturn(EXIST_NAME);
        var expectedLogicalValidationException =
                createLogicalValidationException(NOT_UNIQUE_CODE, format(NOT_UNIQUE_MESSAGE, NAME));
        var groupTicketTypes = GroupTicketTypes.builder().account(new Account()).build();
        var actualLogicalValidationException =
                assertThrows(LogicalValidationException.class, () -> validator.checkUniqueness(groupTicketTypes));
        assertEquals(
                expectedLogicalValidationException.getFieldErrors().get(NOT_UNIQUE_CODE).get(0),
                actualLogicalValidationException.getFieldErrors().get(NOT_UNIQUE_CODE).get(0)
        );
        verify(repository, times(1)).getByNameAndAccount_IdAndIdNot(any(), any(), any());
    }
}
