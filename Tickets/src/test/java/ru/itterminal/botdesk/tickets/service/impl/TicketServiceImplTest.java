package ru.itterminal.botdesk.tickets.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Optional;
import java.util.UUID;
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.test.RoleTestHelper;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.commons.model.spec.SpecificationsFactory;
import ru.itterminal.botdesk.files.service.FileServiceImpl;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.security.jwt.JwtUserBuilder;
import ru.itterminal.botdesk.tickets.model.Ticket;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.test.TicketSettingTestHelper;
import ru.itterminal.botdesk.tickets.model.test.TicketTestHelper;
import ru.itterminal.botdesk.tickets.repository.TicketRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator;

@SpringJUnitConfig(value = {TicketServiceImpl.class})
class TicketServiceImplTest {

    @MockBean
    private TicketRepository repository;

    @SuppressWarnings("unused")
    @MockBean
    private FileServiceImpl fileService;

    @MockBean
    private TicketOperationValidator validator;

    @MockBean
    private TicketCounterServiceImpl counterService;

    @MockBean
    private JwtUserBuilder jwtUserBuilder;

    @MockBean
    private JwtUser jwtUser;

    @MockBean
    private SpecificationsFactory specFactory;

    @Autowired
    private TicketServiceImpl service;

    @MockBean
    private TicketSettingServiceImpl ticketSettingService;

    private final TicketTestHelper ticketTestHelper = new TicketTestHelper();
    private final UserTestHelper userTestHelper = new UserTestHelper();
    private final RoleTestHelper roleTestHelper = new RoleTestHelper();
    private final TicketSettingTestHelper ticketSettingTestHelper = new TicketSettingTestHelper();

    private Ticket ticket;
    private User currentUser;
    private TicketSetting ticketSetting;

    @BeforeEach
    void setUp() {
        ticket = ticketTestHelper.getRandomValidEntity();
        currentUser = userTestHelper.getRandomValidEntity();
        ticketSetting = ticketSettingTestHelper.getRandomValidEntity();
        when(jwtUserBuilder.getJwtUser()).thenReturn(jwtUser);
        when(jwtUser.getAccountId()).thenReturn(UUID.randomUUID());
    }

    @Test
    void create_shouldCreateTicket_whenPassedValidData() {
        ticket.getGroup().setIsInner(false);
        when(validator.beforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(ticketSettingService.getSettingOrPredefinedValuesForTicket(any(), any(), any()))
                .thenReturn(new TicketSetting());
        when(repository.create(any())).thenReturn(ticket);
        when(counterService.getTicketNumber(any())).thenReturn(ticket.getNumber());
        var createdTicket = service.create(ticket, ticket.getAuthor());
        assertEquals(ticket, createdTicket);
        verify(validator, times(1)).beforeCreate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(repository, times(1)).create(any());
        verify(counterService, times(1)).getTicketNumber(any());
    }

    @Test
    void create_shouldUpdateTicket_whenPassedValidData() {
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(repository.existsById(any())).thenReturn(true);
        when(repository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(ticket));
        when(repository.update(any())).thenReturn(ticket);
        var updatedTicket = service.update(ticket, ticket.getAuthor());
        assertEquals(ticket, updatedTicket);
        verify(validator, times(1)).beforeUpdate(any());
        verify(repository, times(1)).existsById(any());
        verify(repository, times(1)).findByIdAndAccountId(any(), any());
        verify(repository, times(1)).update(any());
        verify(counterService, times(0)).getTicketNumber(any());
    }

    @ParameterizedTest
    @MethodSource("getParametersForTestCreateUpdateTicketWithDefaultSettingsOrFromValuesFromDatabase")
    void create_shouldCreateTicketWithDefaultSettings_whenCurrentUserHasOneFromRolesAdminOrExecutorOrAuthorOrObserver
            (String nameOfRole, boolean isInnerGroup) {
        currentUser.getGroup().setIsInner(isInnerGroup);
        Long number = (long) (Math.random() * 1000);
        when(validator.beforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(repository.create(any())).thenReturn(ticket);
        when(counterService.getTicketNumber(any())).thenReturn(number);
        when(ticketSettingService.getSettingOrPredefinedValuesForTicket(any(), any(), any())).thenReturn(ticketSetting);
        currentUser.setRole(roleTestHelper.getRoleByName(nameOfRole));
        var createdTicket = service.create(ticket, currentUser);
        assertEquals(ticket, createdTicket);
        verify(validator, times(1)).beforeCreate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(repository, times(1)).create(any());
        verify(counterService, times(1)).getTicketNumber(any());
        verify(ticketSettingService, times(1)).getSettingOrPredefinedValuesForTicket(any(), any(), any());
        assertEquals(ticket.getTicketStatus(), ticketSetting.getTicketStatusForNew());
        assertEquals(ticket.getTicketType(), ticketSetting.getTicketTypeForNew());
        assertEquals(ticket.getExecutors(), ticketSetting.getExecutors());
        assertEquals(ticket.getObservers(), ticketSetting.getObservers());
        assertEquals(ticket.getGroup(), ticket.getAuthor().getGroup());
        assertEquals(ticket.getNumber(), number);
    }

    @ParameterizedTest
    @MethodSource("getParametersForTestCreateUpdateTicketWithValuesWithoutChanges")
    void create_shouldCreateTicketWithValuesWithoutChanges_whenCurrentUserHasOneFromRolesAccountOwnerOrAdminOrExecutor
            (String nameOfRole, boolean isInnerGroup) {
        currentUser.getGroup().setIsInner(isInnerGroup);
        Long number = (long) (Math.random() * 1000);
        when(validator.beforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(repository.create(any())).thenReturn(ticket);
        when(counterService.getTicketNumber(any())).thenReturn(number);
        when(ticketSettingService.getSettingOrPredefinedValuesForTicket(any(), any(), any())).thenReturn(ticketSetting);
        currentUser.setRole(roleTestHelper.getRoleByName(nameOfRole));
        var createdTicket = service.create(ticket, currentUser);
        assertEquals(ticket, createdTicket);
        verify(validator, times(1)).beforeCreate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(repository, times(1)).create(any());
        verify(counterService, times(1)).getTicketNumber(any());
        verify(ticketSettingService, times(0)).getSettingOrPredefinedValuesForTicket(any(), any(), any());
        assertEquals(ticket.getGroup(), ticket.getAuthor().getGroup());
        assertEquals(ticket.getNumber(), number);
    }

    @ParameterizedTest
    @MethodSource("getParametersForTestCreateUpdateTicketWithDefaultSettingsOrFromValuesFromDatabase")
    void update_shouldUpdateTicketAndSetSomeFieldsFromDataBase_whenCurrentUserHasOneFromRolesAdminOrExecutorOrAuthorOrObserver
            (String nameOfRole, boolean isInnerGroup) {
        currentUser.getGroup().setIsInner(isInnerGroup);
        var ticketFromDataBase = ticketTestHelper.getRandomValidEntity();
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(repository.existsById(any())).thenReturn(true);
        when(repository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(ticketFromDataBase));
        when(repository.update(any())).thenReturn(ticket);
        currentUser.setRole(roleTestHelper.getRoleByName(nameOfRole));
        var updatedTicket = service.update(ticket, currentUser);
        assertEquals(ticket, updatedTicket);
        verify(validator, times(1)).beforeUpdate(any());
        verify(repository, times(1)).existsById(any());
        verify(repository, times(1)).findByIdAndAccountId(any(), any());
        verify(repository, times(1)).update(any());
        verify(counterService, times(0)).getTicketNumber(any());
        assertEquals(updatedTicket.getNumber(), ticketFromDataBase.getNumber());
        assertEquals(updatedTicket.getCreatedAt(), ticketFromDataBase.getCreatedAt());
        assertEquals(updatedTicket.getFiles(), ticketFromDataBase.getFiles());
        assertEquals(updatedTicket.getTicketTemplate(), ticketFromDataBase.getTicketTemplate());
        assertEquals(ticket.getGroup(), ticket.getAuthor().getGroup());
        assertEquals(updatedTicket.getObservers(), ticketFromDataBase.getObservers());
        assertEquals(updatedTicket.getExecutors(), ticketFromDataBase.getExecutors());
    }

    @ParameterizedTest
    @MethodSource("getParametersForTestCreateUpdateTicketWithValuesWithoutChanges")
    void update_shouldUpdateTicketWithValuesWithoutChanges_whenCurrentUserHasOneOfRolesAccountOwnerOrAdminOrExecutor
            (String nameOfRole, boolean isInnerGroup) {
        currentUser.getGroup().setIsInner(isInnerGroup);
        var ticketFromDataBase = ticketTestHelper.getRandomValidEntity();
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(repository.existsById(any())).thenReturn(true);
        when(repository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(ticketFromDataBase));
        when(repository.update(any())).thenReturn(ticket);
        currentUser.setRole(roleTestHelper.getRoleByName(nameOfRole));
        var updatedTicket = service.update(ticket, currentUser);
        assertEquals(ticket, updatedTicket);
        verify(validator, times(1)).beforeUpdate(any());
        verify(repository, times(1)).existsById(any());
        verify(repository, times(1)).findByIdAndAccountId(any(), any());
        verify(repository, times(1)).update(any());
        verify(counterService, times(0)).getTicketNumber(any());
        assertEquals(updatedTicket.getNumber(), ticketFromDataBase.getNumber());
        assertEquals(updatedTicket.getCreatedAt(), ticketFromDataBase.getCreatedAt());
        assertEquals(updatedTicket.getFiles(), ticketFromDataBase.getFiles());
        assertEquals(updatedTicket.getTicketTemplate(), ticketFromDataBase.getTicketTemplate());
        assertEquals(ticket.getGroup(), ticket.getAuthor().getGroup());
        assertEquals(updatedTicket.getObservers(), updatedTicket.getObservers());
        assertEquals(updatedTicket.getExecutors(), updatedTicket.getExecutors());
    }

    private static Stream<Arguments> getParametersForTestCreateUpdateTicketWithValuesWithoutChanges() {
        return Stream.of(
                Arguments.of(Roles.ACCOUNT_OWNER.toString(), true),
                Arguments.of(Roles.ADMIN.toString(), true),
                Arguments.of(Roles.EXECUTOR.toString(), true)
        );
    }

    private static Stream<Arguments> getParametersForTestCreateUpdateTicketWithDefaultSettingsOrFromValuesFromDatabase() {
        return Stream.of(
                Arguments.of(Roles.ADMIN.toString(), false),
                Arguments.of(Roles.EXECUTOR.toString(), false),
                Arguments.of(Roles.AUTHOR.toString(), false),
                Arguments.of(Roles.OBSERVER.toString(), true)
        );
    }

}