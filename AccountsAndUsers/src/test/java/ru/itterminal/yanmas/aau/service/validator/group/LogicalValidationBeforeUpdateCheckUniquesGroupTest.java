package ru.itterminal.yanmas.aau.service.validator.group;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.projection.GroupUniqueFields;
import ru.itterminal.yanmas.aau.model.test.GroupTestHelper;
import ru.itterminal.yanmas.aau.repository.GroupRepository;
import ru.itterminal.yanmas.commons.exception.LogicalValidationException;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;

import java.util.List;

import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;

@SpringJUnitConfig(value = {LogicalValidationBeforeUpdateCheckUniquesGroup.class})
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class LogicalValidationBeforeUpdateCheckUniquesGroupTest {

    @MockBean
    private GroupRepository repository;

    @MockBean
    private GroupUniqueFields groupUniqueFields;

    @Autowired
    private final LogicalValidationBeforeUpdateCheckUniquesGroup logicalValidationBeforeUpdateCheckUniquesGroup =
            new LogicalValidationBeforeUpdateCheckUniquesGroup(repository);

    private final GroupTestHelper groupTestHelper = new GroupTestHelper();

    @Test
    void logicalValidationBeforeUpdate_shouldGetLogicalValidationException_whenNameNotUnique() {
        Group group = groupTestHelper.getRandomValidEntity();
        var errors = createMapForLogicalErrors();
        errors.put("name", singletonList(new ValidationError("name", group.getName() + " is occupied")));
        when(repository.getByNameAndIsInnerAndAccount_IdAndIdNot(any(), any(), any(), any())).thenReturn(List.of(groupUniqueFields));
        when(groupUniqueFields.getName()).thenReturn(group.getName());
        LogicalValidationException logicalValidationException = new LogicalValidationException("name", errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> logicalValidationBeforeUpdateCheckUniquesGroup.logicalValidationBeforeUpdate(group, errors));
        assertEquals(logicalValidationException.getFieldErrors().get("name").get(0),
                thrown.getFieldErrors().get("name").get(0));
        verify(repository, times(1)).getByNameAndIsInnerAndAccount_IdAndIdNot(any(), any(), any(), any());
    }


}