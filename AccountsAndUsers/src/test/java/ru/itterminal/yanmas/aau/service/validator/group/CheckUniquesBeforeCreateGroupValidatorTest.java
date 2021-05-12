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
import ru.itterminal.yanmas.aau.service.validator.group.logical_validation.CheckUniquesBeforeCreateGroupValidator;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;

@SpringJUnitConfig(value = {CheckUniquesBeforeCreateGroupValidator.class})
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class CheckUniquesBeforeCreateGroupValidatorTest {
    @MockBean
    private GroupRepository repository;

    @MockBean
    private GroupUniqueFields groupUniqueFields;

    @Autowired
    private final CheckUniquesBeforeCreateGroupValidator checkUniquesBeforeCreateGroupValidator =
            new CheckUniquesBeforeCreateGroupValidator(repository);

    private final GroupTestHelper groupTestHelper = new GroupTestHelper();

    @Test
    void logicalValidationBeforeCreate_shouldAddErrorToMap_whenNameNotUnique() {
        Group group = groupTestHelper.getRandomValidEntity();
        var errors = createMapForLogicalErrors();
        when(repository.getByNameAndIsInnerAndAccount_Id(any(), any(), any())).thenReturn(List.of(groupUniqueFields));
        assertEquals(0, errors.values().size());
        checkUniquesBeforeCreateGroupValidator.logicalValidationBeforeCreate(group, errors);
        assertEquals(1, errors.values().size());
        verify(repository, times(1)).getByNameAndIsInnerAndAccount_Id(any(), any(), any());
    }

    @Test
    void logicalValidationBeforeCreate_shouldNotAddErrorToMap_whenNameUnique() {
        Group group = groupTestHelper.getRandomValidEntity();
        var errors = createMapForLogicalErrors();
        when(repository.getByNameAndIsInnerAndAccount_Id(any(), any(), any())).thenReturn(List.of());
        assertEquals(0, errors.values().size());
        checkUniquesBeforeCreateGroupValidator.logicalValidationBeforeCreate(group, errors);
        assertEquals(0, errors.values().size());
        verify(repository, times(1)).getByNameAndIsInnerAndAccount_Id(any(), any(), any());
    }
}