package ru.itterminal.yanmas.tickets.service.validator;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.aau.model.Account;
import ru.itterminal.yanmas.commons.exception.LogicalValidationException;
import ru.itterminal.yanmas.security.config.TestSecurityConfig;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;
import ru.itterminal.yanmas.tickets.model.TicketSetting;
import ru.itterminal.yanmas.tickets.model.test.TicketSettingTestHelper;
import ru.itterminal.yanmas.tickets.repository.TicketSettingRepository;
import ru.itterminal.yanmas.tickets.service.impl.SettingsAccessToTicketTypesServiceImpl;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl.NOT_UNIQUE_CODE;
import static ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl.NOT_UNIQUE_MESSAGE;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createLogicalValidationException;
import static ru.itterminal.yanmas.security.jwt.JwtUserBuilder.USER_FROM_OUTER_GROUP_CANNOT_CREATE_OR_UPDATE_THIS_ENTITY;

@SpringJUnitConfig(value = {TicketSettingOperationValidator.class, JwtUserBuilder.class})
@Import(TestSecurityConfig.class)
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class TicketSettingOperationValidatorTest {

    @MockBean
    private TicketSettingRepository repository;

    @MockBean
    private SettingsAccessToTicketTypesServiceImpl settingsAccessToTicketTypesService;

    @SuppressWarnings("unused")
    @Autowired
    private JwtUserBuilder jwtUserBuilder;

    @Autowired
    private TicketSettingOperationValidator validator;

    private final TicketSettingTestHelper ticketSettingTestHelper = new TicketSettingTestHelper();

    @Test
    void checkUniqueness_shouldGetTrue_whenTicketSettingIsUnique() {
        when(repository.findAllByAccount_IdAndGroup_IdAndAuthor_IdAndIdNot(any(), any(), any(), any())).thenReturn(Collections.emptyList());
        assertTrue(validator.checkUniqueness(ticketSettingTestHelper.getRandomValidEntity()));
        verify(repository, times(1)).findAllByAccount_IdAndGroup_IdAndAuthor_IdAndIdNot(any(), any(), any(), any());

    }

    @Test
    void checkUniqueness_shouldGetLogicalValidationException_whenPassedDataNotUnique() {
        when(repository.findAllByAccount_IdAndGroup_IdAndAuthor_IdAndIdNot(any(), any(), any(), any())).thenReturn(List.of(new TicketSetting()));
        var expectedException =
                createLogicalValidationException(NOT_UNIQUE_CODE, String.format(NOT_UNIQUE_MESSAGE, TicketSettingOperationValidator.TICKET_SETTING_UNIQUE_FIELDS));
        var ticketSetting = ticketSettingTestHelper.getRandomValidEntity();
        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.checkUniqueness(ticketSetting)
                );
        assertEquals(
                expectedException.getFieldErrors().get(NOT_UNIQUE_CODE).get(0),
                actualException.getFieldErrors().get(NOT_UNIQUE_CODE).get(0)
        );
        verify(repository, times(1)).findAllByAccount_IdAndGroup_IdAndAuthor_IdAndIdNot(any(), any(), any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetTrue_whenPassedRandomValidEntity() {
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(true);
        TicketSetting ticketSetting = ticketSettingTestHelper.getRandomValidEntity();
        assertTrue(validator.logicalValidationBeforeCreate(ticketSetting));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetTrue_whenPassedPredefinedValidEntity() {
        TicketSetting ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(true);
        assertTrue(validator.logicalValidationBeforeCreate(ticketSetting));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetTrue_whenPassedRandomValidEntity() {
        when(repository.findAllByAccount_IdAndGroup_IdAndAuthor_IdAndIdNot(any(), any(), any(), any())).thenReturn(Collections.emptyList());
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(true);
        TicketSetting ticketSetting = ticketSettingTestHelper.getRandomValidEntity();
        assertTrue(validator.logicalValidationBeforeUpdate(ticketSetting));
        verify(repository, times(1)).findAllByAccount_IdAndGroup_IdAndAuthor_IdAndIdNot(any(), any(), any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetTrue_whenPassedPredefinedValidEntity() {
        when(repository.findAllByAccount_IdAndGroup_IdAndAuthor_IdAndIdNot(any(), any(), any(), any())).thenReturn(Collections.emptyList());
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(true);
        TicketSetting ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        assertTrue(validator.logicalValidationBeforeUpdate(ticketSetting));
        verify(repository, times(1)).findAllByAccount_IdAndGroup_IdAndAuthor_IdAndIdNot(any(), any(), any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenAllPassedSettingIsNull() {
        when(repository.findAllByAccount_IdAndGroup_IdAndAuthor_IdAndIdNot(any(), any(), any(), any())).thenReturn(Collections.emptyList());
        TicketSetting ticketSetting = new TicketSetting();
        ticketSetting.setAccount(new Account());
        LogicalValidationException exception = assertThrows(LogicalValidationException.class,
                () -> validator.logicalValidationBeforeUpdate(ticketSetting));
        assertEquals(1, exception.getFieldErrors().get(TicketSettingOperationValidator.INVALID_TICKET_SETTINGS).size());
        verify(repository, times(1)).findAllByAccount_IdAndGroup_IdAndAuthor_IdAndIdNot(any(), any(), any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void checkAccessBeforeCreate_shouldGetAccessDeniedException_whenCurrentUserFromNotInnerGroup() {
        var ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        AccessDeniedException thrown = assertThrows(AccessDeniedException.class,
                () -> validator.checkAccessBeforeCreate(ticketSetting));
        assertEquals(USER_FROM_OUTER_GROUP_CANNOT_CREATE_OR_UPDATE_THIS_ENTITY,
                thrown.getMessage());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void checkAccessBeforeUpdate_shouldGetAccessDeniedException_whenCurrentUserFromNotInnerGroup() {
        var ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        AccessDeniedException thrown = assertThrows(AccessDeniedException.class,
                () -> validator.checkAccessBeforeUpdate(ticketSetting));
        assertEquals(USER_FROM_OUTER_GROUP_CANNOT_CREATE_OR_UPDATE_THIS_ENTITY,
                thrown.getMessage());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetTrue_whenCurrentUserFromInnerGroup() {
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(true);
        var ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        assertTrue(validator.logicalValidationBeforeCreate(ticketSetting));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetTrue_whenCurrentUserFromInnerGroup() {
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(true);
        var ticketSetting = ticketSettingTestHelper.getPredefinedValidEntityList().get(0);
        assertTrue(validator.logicalValidationBeforeUpdate(ticketSetting));
    }

}
