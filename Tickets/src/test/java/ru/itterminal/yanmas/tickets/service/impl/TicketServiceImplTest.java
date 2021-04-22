package ru.itterminal.yanmas.tickets.service.impl;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.model.test.RoleTestHelper;
import ru.itterminal.yanmas.aau.model.test.UserTestHelper;
import ru.itterminal.yanmas.aau.service.impl.AccountServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.files.service.FileServiceImpl;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.model.TicketSetting;
import ru.itterminal.yanmas.tickets.model.test.TicketSettingTestHelper;
import ru.itterminal.yanmas.tickets.model.test.TicketTestHelper;
import ru.itterminal.yanmas.tickets.repository.TicketRepository;
import ru.itterminal.yanmas.tickets.service.validator.TicketOperationValidator;

import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@SuppressWarnings("unused")
@SpringJUnitConfig(value = {TicketServiceImpl.class})
class TicketServiceImplTest {

    @MockBean
    private TicketRepository repository;

    @SuppressWarnings("unused")
    @MockBean
    private FileServiceImpl fileService;

    @SuppressWarnings("unused")
    @MockBean
    private JwtUserBuilder jwtUserBuilder;

    @MockBean
    private TicketOperationValidator validator;

    @MockBean
    private TicketCounterServiceImpl ticketCounterService;

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
    private TicketServiceImpl ticketService;

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
    }

    @Test
    void create_shouldCreateTicket_whenPassedValidData() {
        ticket.getGroup().setIsInner(false);
        when(validator.logicalValidationBeforeCreate(any())).thenReturn(true);
        when(accountService.findById(any())).thenReturn(ticket.getAccount());
        when(userService.findByIdAndAccountId(any(), any())).thenReturn(ticket.getAuthor());
        when(ticketSettingService.getSettingOrPredefinedValuesForTicket(any(), any(), any()))
                .thenReturn(new TicketSetting());
        when(repository.create(any())).thenReturn(ticket);
        when(ticketCounterService.getNextTicketNumber(any())).thenReturn(ticket.getNumber());
        var createdTicket = ticketService.create(ticket, ticket.getAuthor());
        Assertions.assertEquals(ticket, createdTicket);
        verify(accountService, times(1)).findById(any());
        verify(userService, times(1)).findByIdAndAccountId(any(), any());
        verify(validator, times(1)).logicalValidationBeforeCreate(any());
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
        when(userService.findByIdAndAccountId(any(), any())).thenReturn(ticket.getAuthor());
        when(validator.logicalValidationBeforeCreate(any())).thenReturn(true);
        when(repository.create(any())).thenReturn(ticket);
        when(ticketCounterService.getNextTicketNumber(any())).thenReturn(number);
        when(ticketSettingService.getSettingOrPredefinedValuesForTicket(any(), any(), any())).thenReturn(ticketSetting);
        currentUser.setRole(roleTestHelper.getRoleByName(nameOfRole));
        var createdTicket = ticketService.create(ticket, currentUser);
        Assertions.assertEquals(ticket, createdTicket);
        verify(accountService, times(1)).findById(any());
        verify(userService, times(1)).findByIdAndAccountId(any(), any());
        verify(validator, times(1)).logicalValidationBeforeCreate(any());
        verify(repository, times(1)).create(any());
        verify(ticketCounterService, times(1)).getNextTicketNumber(any());
        verify(ticketSettingService, times(1)).getSettingOrPredefinedValuesForTicket(any(), any(), any());
        assertEquals(createdTicket.getTicketStatus(), ticketSetting.getTicketStatusForNew());
        Assertions.assertEquals(createdTicket.getTicketType(), ticketSetting.getTicketTypeForNew());
        Assertions.assertEquals(createdTicket.getExecutors(), ticketSetting.getExecutors());
        Assertions.assertEquals(createdTicket.getObservers(), ticketSetting.getObservers());
        Assertions.assertEquals(createdTicket.getGroup(), ticket.getAuthor().getGroup());
        Assertions.assertEquals(createdTicket.getNumber(), number);
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
        when(userService.findByIdAndAccountId(any(), any())).thenReturn(currentUser);
        when(validator.logicalValidationBeforeCreate(any())).thenReturn(true);
        when(repository.create(any())).thenReturn(ticket);
        when(ticketCounterService.getNextTicketNumber(any())).thenReturn(number);
        currentUser.setRole(roleTestHelper.getRoleByName(nameOfRole));
        var createdTicket = ticketService.create(ticket, currentUser);
        Assertions.assertEquals(ticket, createdTicket);
        verify(accountService, times(1)).findById(any());
        verify(userService, times(1)).findByIdAndAccountId(any(), any());
        verify(validator, times(1)).logicalValidationBeforeCreate(any());
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
        when(userService.findByIdAndAccountId(any(), any())).thenReturn(ticket.getAuthor());
        when(validator.logicalValidationBeforeCreate(any())).thenReturn(true);
        when(repository.create(any())).thenReturn(ticket);
        when(ticketCounterService.getNextTicketNumber(any())).thenReturn(number);
        when(ticketSettingService.getSettingOrPredefinedValuesForTicket(any(), any(), any())).thenReturn(ticketSetting);
        currentUser.setRole(roleTestHelper.getRoleByName(nameOfRole));
        var createdTicket = ticketService.create(ticket, currentUser);
        Assertions.assertEquals(ticket, createdTicket);
        verify(accountService, times(1)).findById(any());
        verify(userService, times(1)).findByIdAndAccountId(any(), any());
        verify(validator, times(1)).logicalValidationBeforeCreate(any());
        verify(repository, times(1)).create(any());
        verify(ticketCounterService, times(1)).getNextTicketNumber(any());
        verify(ticketSettingService, times(1)).getSettingOrPredefinedValuesForTicket(any(), any(), any());
        assertEquals(ticket.getGroup(), ticket.getAuthor().getGroup());
        assertEquals(ticket.getNumber(), number);
    }

    @Test
    void update_shouldUpdateTicket_whenPassedValidData() {
        when(validator.logicalValidationBeforeUpdate(any())).thenReturn(true);
        when(accountService.findById(any())).thenReturn(ticket.getAccount());
        when(userService.findByIdAndAccountId(any(), any())).thenReturn(ticket.getAuthor());
        when(ticketSettingService.getSettingOrPredefinedValuesForTicket(any(), any(), any()))
                .thenReturn(new TicketSetting());
        when(repository.update(any())).thenReturn(ticket);
        when(repository.existsById(any())).thenReturn(true);
        when(repository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(ticket));
        var updatedTicket = ticketService.update(ticket, ticket.getAuthor());
        Assertions.assertEquals(ticket, updatedTicket);
        verify(accountService, times(1)).findById(any());
        verify(userService, times(1)).findByIdAndAccountId(any(), any());
        verify(validator, times(1)).logicalValidationBeforeUpdate(any());
        verify(repository, times(1)).update(any());
        verify(repository, times(1)).existsById(any());
        verify(repository, times(1)).findByIdAndAccountId(any(), any());
        verify(ticketCounterService, times(0)).getNextTicketNumber(any());
    }

    @Test
    void reOpen_shouldReOpenTicket_whenPassedValidData() {
        when(validator.logicalValidationBeforeUpdate(any())).thenReturn(true);
        when(ticketSettingService.getSettingOrPredefinedValuesForTicket(any(), any(), any()))
                .thenReturn(ticketSettingTestHelper.getRandomValidEntity());
        when(repository.update(any())).thenReturn(ticket);
        var updatedTicket = ticketService.reOpen(ticket);
        Assertions.assertEquals(ticket, updatedTicket);
        verify(validator, times(1)).logicalValidationBeforeUpdate(any());
        verify(repository, times(1)).update(any());
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
