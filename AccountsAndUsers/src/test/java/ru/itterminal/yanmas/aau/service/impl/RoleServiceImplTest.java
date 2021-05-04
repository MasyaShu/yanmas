package ru.itterminal.yanmas.aau.service.impl;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.aau.model.Role;
import ru.itterminal.yanmas.aau.model.test.RoleTestHelper;
import ru.itterminal.yanmas.aau.repository.RoleRepository;
import ru.itterminal.yanmas.aau.service.validator.account.RoleOperationValidator;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {RoleServiceImpl.class})
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class RoleServiceImplTest {

    private final RoleTestHelper roleTestHelper = new RoleTestHelper();

    @MockBean
    private RoleRepository repository;

    @SuppressWarnings("unused")
    @MockBean
    @Qualifier("basicOperationValidatorImpl")
    private RoleOperationValidator validator;

    @Autowired
    private RoleServiceImpl service;

    private final Role role = roleTestHelper.getPredefinedValidEntityList().get(0);


    @Test
    void getAccountOwnerRole_shouldGetRole_whenPassedValidData() {
        when(repository.getByName(any())).thenReturn(Optional.of(role));
        Role findRole = service.getAccountOwnerRole();
        assertEquals(findRole, role);
        verify(repository, times(1)).getByName(any());
    }
}
