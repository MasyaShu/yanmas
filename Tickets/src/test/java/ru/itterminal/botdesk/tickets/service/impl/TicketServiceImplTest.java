package ru.itterminal.botdesk.tickets.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;
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
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
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

@SuppressWarnings("unused")
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
    private TicketCounterServiceImpl ticketCounterService;

    @MockBean
    private JwtUserBuilder jwtUserBuilder;

    @MockBean
    private JwtUser jwtUser;

    @MockBean
    private SpecificationsFactory specFactory;

    @MockBean
    private TicketTypeServiceImpl ticketTypeService;

    @MockBean
    private TicketStatusServiceImpl ticketStatusService;

    @MockBean
    private UserServiceImpl userService;

    @MockBean
    private AccountServiceImpl accountService;

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
        when(accountService.findById(any())).thenReturn(ticket.getAccount());
        when(userService.findByIdAndAccountId(any())).thenReturn(ticket.getAuthor());
        when(ticketSettingService.getSettingOrPredefinedValuesForTicket(any(), any(), any()))
                .thenReturn(new TicketSetting());
        when(repository.create(any())).thenReturn(ticket);
        when(ticketCounterService.getNextTicketNumber(any())).thenReturn(ticket.getNumber());
        var createdTicket = service.create(ticket, ticket.getAuthor());
        assertEquals(ticket, createdTicket);
        verify(accountService, times(1)).findById(any());
        verify(userService, times(1)).findByIdAndAccountId(any());
        verify(validator, times(1)).beforeCreate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(repository, times(1)).create(any());
        verify(ticketCounterService, times(1)).getNextTicketNumber(any());
    }

    @ParameterizedTest
    @MethodSource("getStreamOfUsersFromOuterGroupsWithRolesAdminOrExecutorOrAuthor")
    void create_shouldCreateTicketWithValuesFromSettings_whenCurrentFromOuterGroupsWithRolesAdminOrExecutorOrAuthor
            (String nameOfRole, boolean isInnerGroup) {
        currentUser.getGroup().setIsInner(isInnerGroup);
        Long number = (long) (Math.random() * 1000);
        ticket.setNumber(number);
        ticket.setIsFinished(false);
        when(accountService.findById(any())).thenReturn(ticket.getAccount());
        when(userService.findByIdAndAccountId(any())).thenReturn(ticket.getAuthor());
        when(validator.beforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(repository.create(any())).thenReturn(ticket);
        when(ticketCounterService.getNextTicketNumber(any())).thenReturn(number);
        when(ticketSettingService.getSettingOrPredefinedValuesForTicket(any(), any(), any())).thenReturn(ticketSetting);
        currentUser.setRole(roleTestHelper.getRoleByName(nameOfRole));
        var createdTicket = service.create(ticket, currentUser);
        assertEquals(ticket, createdTicket);
        verify(accountService, times(1)).findById(any());
        verify(userService, times(1)).findByIdAndAccountId(any());
        verify(validator, times(1)).beforeCreate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(repository, times(1)).create(any());
        verify(ticketCounterService, times(1)).getNextTicketNumber(any());
        verify(ticketSettingService, times(1)).getSettingOrPredefinedValuesForTicket(any(), any(), any());
        assertEquals(createdTicket.getTicketStatus(), ticketSetting.getTicketStatusForNew());
        assertEquals(createdTicket.getTicketType(), ticketSetting.getTicketTypeForNew());
        assertEquals(createdTicket.getExecutors(), ticketSetting.getExecutors());
        assertEquals(createdTicket.getObservers(), ticketSetting.getObservers());
        assertEquals(createdTicket.getGroup(), ticket.getAuthor().getGroup());
        assertEquals(createdTicket.getNumber(), number);
    }

    @ParameterizedTest
    @MethodSource("getStreamOfUsersFromInnerGroupsWithRolesAccountOwnerOrAdminOrExecutor")
    void create_shouldCreateTicketWithValuesFromRequest_whenCurrentUserUsersFromInnerGroupsWithRolesAccountOwnerOrAdminOrExecutor
            (String nameOfRole, boolean isInnerGroup) {
        currentUser.getGroup().setIsInner(isInnerGroup);
        Long number = (long) (Math.random() * 1000);
        ticket.setNumber(number);
        ticket.setIsFinished(false);
        ticket.setExecutors(List.of(currentUser));
        ticket.setObservers(List.of(ticket.getAuthor()));
        when(accountService.findById(any())).thenReturn(ticket.getAccount());
        when(userService.findByIdAndAccountId(any())).thenReturn(ticket.getAuthor());
        when(validator.beforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(repository.create(any())).thenReturn(ticket);
        when(ticketCounterService.getNextTicketNumber(any())).thenReturn(number);
        currentUser.setRole(roleTestHelper.getRoleByName(nameOfRole));
        var createdTicket = service.create(ticket, currentUser);
        assertEquals(ticket, createdTicket);
        verify(accountService, times(1)).findById(any());
        verify(userService, times(1)).findByIdAndAccountId(any());
        verify(validator, times(1)).beforeCreate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(repository, times(1)).create(any());
        verify(ticketCounterService, times(1)).getNextTicketNumber(any());
        verify(ticketSettingService, times(1)).getSettingOrPredefinedValuesForTicket(any(), any(), any());
        assertEquals(ticket.getGroup(), ticket.getAuthor().getGroup());
        assertEquals(ticket.getNumber(), number);
    }


    @ParameterizedTest
    @MethodSource("getStreamOfUsersFromInnerGroupsWithRolesAccountOwnerOrAdminOrExecutor")
    void create_shouldCreateTicketWithTicketStatusForClose_whenIsFinishedIsTrueAndUsersFromInnerGroupsWithRolesAccountOwnerOrAdminOrExecutor
            (String nameOfRole, boolean isInnerGroup) {
        currentUser.getGroup().setIsInner(isInnerGroup);
        Long number = (long) (Math.random() * 1000);
        ticket.setNumber(number);
        ticket.setIsFinished(true);
        ticket.setExecutors(List.of(currentUser));
        ticket.setObservers(List.of(ticket.getAuthor()));
        ticket.setTicketStatus(ticketSetting.getTicketStatusForClose());
        when(accountService.findById(any())).thenReturn(ticket.getAccount());
        when(userService.findByIdAndAccountId(any())).thenReturn(ticket.getAuthor());
        when(validator.beforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(repository.create(any())).thenReturn(ticket);
        when(ticketCounterService.getNextTicketNumber(any())).thenReturn(number);
        when(ticketSettingService.getSettingOrPredefinedValuesForTicket(any(), any(), any())).thenReturn(ticketSetting);
        currentUser.setRole(roleTestHelper.getRoleByName(nameOfRole));
        var createdTicket = service.create(ticket, currentUser);
        assertEquals(ticket, createdTicket);
        verify(accountService, times(1)).findById(any());
        verify(userService, times(1)).findByIdAndAccountId(any());
        verify(validator, times(1)).beforeCreate(any());
        verify(validator, times(1)).checkUniqueness(any());
        verify(repository, times(1)).create(any());
        verify(ticketCounterService, times(1)).getNextTicketNumber(any());
        verify(ticketSettingService, times(1)).getSettingOrPredefinedValuesForTicket(any(), any(), any());
        assertEquals(ticket.getGroup(), ticket.getAuthor().getGroup());
        assertEquals(ticket.getNumber(), number);
    }

    private static Stream<Arguments> getStreamOfUsersFromInnerGroupsWithRolesAccountOwnerOrAdminOrExecutor() {
        return Stream.of(
                Arguments.of(Roles.ACCOUNT_OWNER.toString(), true),
                Arguments.of(Roles.ADMIN.toString(), true),
                Arguments.of(Roles.EXECUTOR.toString(), true)
        );
    }

    private static Stream<Arguments> getStreamOfUsersFromOuterGroupsWithRolesAdminOrExecutorOrAuthor() {
        return Stream.of(
                Arguments.of(Roles.ADMIN.toString(), false),
                Arguments.of(Roles.EXECUTOR.toString(), false),
                Arguments.of(Roles.AUTHOR.toString(), false)
        );
    }

}