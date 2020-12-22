package ru.itterminal.botdesk.aau.service.impl;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.test.GroupTestHelper;
import ru.itterminal.botdesk.aau.repository.GroupRepository;
import ru.itterminal.botdesk.aau.service.validator.GroupOperationValidator;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;

import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {GroupServiceImpl.class})
@Import(TestSecurityConfig.class)
@TestPropertySource(locations = "/application.properties")
@ActiveProfiles("Test")
class GroupServiceImplTest {

    @MockBean
    private GroupRepository repository;

    @MockBean
    private GroupOperationValidator validator;

    @Autowired
    private GroupServiceImpl service;

    private final GroupTestHelper groupTestHelper = new GroupTestHelper();


    @Test
    void update_shouldUpdateIsInnerFromDataBase_whenPassedIsInnerNull() {
        Group group = groupTestHelper.getRandomValidEntity();
        group.setId(UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"));
        group.setIsInner(null);
        Group groupInDataBase = groupTestHelper.getEntityFromPredefinedValidEntityByEntityId("0223e51a-4bb2-44ee-bc8e-1f047a2145e7");
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(repository.existsById(any())).thenReturn(true);
        //noinspection ConstantConditions
        when(repository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(groupInDataBase));
        when(repository.update(any())).thenReturn(group);
        Group createdGroup = service.update(group);
        assertEquals(createdGroup.getIsInner(), group.getIsInner());
        verify(validator, times(1)).beforeUpdate(any());
        verify(repository, times(2)).existsById(any());
        verify(repository, times(1)).findByIdAndAccountId(any(), any());
        verify(repository, times(1)).update(any());
    }
}