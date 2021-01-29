package ru.itterminal.botdesk.tickets.service.impl;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.test.RoleTestHelper;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.tickets.model.Ticket;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.test.TicketSettingTestHelper;
import ru.itterminal.botdesk.tickets.model.test.TicketTestHelper;
import ru.itterminal.botdesk.tickets.repository.TicketRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@SpringJUnitConfig(value = {TicketServiceImpl.class})
class TicketServiceImplTest {

    @MockBean
    private TicketRepository repository;

    @MockBean
    private TicketOperationValidator validator;

    @MockBean
    private TicketCounterServiceImpl counterService;

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
    }

    @Test
    void create_shouldCreateTicket_whenPassedValidData() {
        ticket.getGroup().setIsInner(false);
        when(validator.beforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(ticketSettingService.getSettingOrPredefinedValuesForTicket(any(), any(), any())).thenReturn(new TicketSetting());
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

    @Test
    void create_shouldCreateTicketAndSetTicketSetting_whenPassedValidDataCurrentUserAuthorInnerGroup() {
        currentUser.getGroup().setIsInner(true);
        currentUser.setRole(roleTestHelper.setPredefinedValidEntityList().get(3));
        Long number = (long) (Math.random() * 1000);
        when(validator.beforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(repository.create(any())).thenReturn(ticket);
        when(counterService.getTicketNumber(any())).thenReturn(number);
        when(ticketSettingService.getSettingOrPredefinedValuesForTicket(any(), any(), any())).thenReturn(ticketSetting);
        var updatedTicket = service.create(ticket, currentUser);
        assertEquals(ticket, updatedTicket);
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

    @Test
    void create_shouldCreateTicketAndSetTicketSetting_whenCurrentUserAdminExecutorAuthorNotInnerGroup() {
        currentUser.getGroup().setIsInner(false);
        Long number = (long) (Math.random() * 1000);
        when(validator.beforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(repository.create(any())).thenReturn(ticket);
        when(counterService.getTicketNumber(any())).thenReturn(number);
        when(ticketSettingService.getSettingOrPredefinedValuesForTicket(any(), any(), any())).thenReturn(ticketSetting);
        for (int i = 1; i <= 3; i++) {
            currentUser.setRole(roleTestHelper.setPredefinedValidEntityList().get(1));
            var updatedTicket = service.create(ticket, currentUser);
            assertEquals(ticket, updatedTicket);
            verify(validator, times(i)).beforeCreate(any());
            verify(validator, times(i)).checkUniqueness(any());
            verify(repository, times(i)).create(any());
            verify(counterService, times(i)).getTicketNumber(any());
            verify(ticketSettingService, times(i)).getSettingOrPredefinedValuesForTicket(any(), any(), any());
            assertEquals(ticket.getTicketStatus(), ticketSetting.getTicketStatusForNew());
            assertEquals(ticket.getTicketType(), ticketSetting.getTicketTypeForNew());
            assertEquals(ticket.getExecutors(), ticketSetting.getExecutors());
            assertEquals(ticket.getObservers(), ticketSetting.getObservers());
            assertEquals(ticket.getGroup(), ticket.getAuthor().getGroup());
            assertEquals(ticket.getNumber(), number);
        }
    }

    @Test
    void create_shouldCreateTicketAndSetTicketSetting_whenCurrentUserAuthorNotInnerGroup() {
        currentUser.getGroup().setIsInner(false);
        currentUser.setRole(roleTestHelper.setPredefinedValidEntityList().get(3));
        Long number = (long) (Math.random() * 1000);
        when(validator.beforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(repository.create(any())).thenReturn(ticket);
        when(counterService.getTicketNumber(any())).thenReturn(number);
        when(ticketSettingService.getSettingOrPredefinedValuesForTicket(any(), any(), any())).thenReturn(ticketSetting);
        var updatedTicket = service.create(ticket, currentUser);
        assertEquals(ticket, updatedTicket);
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

    @Test
    void create_shouldCreateTicket_whenPassedValidDataCurrentUserAccountOwnerAdminExecutorInnerGroup() {
        currentUser.getGroup().setIsInner(true);
        Long number = (long) (Math.random() * 1000);
        when(validator.beforeCreate(any())).thenReturn(true);
        when(validator.checkUniqueness(any())).thenReturn(true);
        when(repository.create(any())).thenReturn(ticket);
        when(counterService.getTicketNumber(any())).thenReturn(number);
        when(ticketSettingService.getSettingOrPredefinedValuesForTicket(any(), any(), any())).thenReturn(ticketSetting);
        for (int i = 0; i <= 2; i++) {
            currentUser.setRole(roleTestHelper.setPredefinedValidEntityList().get(i));
            var updatedTicket = service.create(ticket, currentUser);
            assertEquals(ticket, updatedTicket);
            verify(validator, times(i + 1)).beforeCreate(any());
            verify(validator, times(i + 1)).checkUniqueness(any());
            verify(repository, times(i + 1)).create(any());
            verify(counterService, times(i + 1)).getTicketNumber(any());
            verify(ticketSettingService, times(0)).getSettingOrPredefinedValuesForTicket(any(), any(), any());
            assertEquals(ticket.getGroup(), ticket.getAuthor().getGroup());
            assertEquals(ticket.getNumber(), number);
        }
    }


    ////////////////////////

    @Test
    void update_shouldUpdateTicketAndSetFieldsFromDataBase_whenPassedValidDataCurrentUserAuthorInnerGroup() {
        currentUser.getGroup().setIsInner(true);
        currentUser.setRole(roleTestHelper.setPredefinedValidEntityList().get(3));
        var ticketFromDataBase = ticketTestHelper.getRandomValidEntity();
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(repository.existsById(any())).thenReturn(true);
        when(repository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(ticketFromDataBase));
        when(repository.update(any())).thenReturn(ticket);

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

    @Test
    void update_shouldUpdateTicketAndSetFieldsFromDataBase_whenCurrentUserAdminExecutorAuthorNotInnerGroup() {
        currentUser.getGroup().setIsInner(true);
        var ticketFromDataBase = ticketTestHelper.getRandomValidEntity();
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(repository.existsById(any())).thenReturn(true);
        when(repository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(ticketFromDataBase));
        when(repository.update(any())).thenReturn(ticket);
        for (int i = 1; i <= 3; i++) {
            currentUser.setRole(roleTestHelper.setPredefinedValidEntityList().get(i));
            var updatedTicket = service.update(ticket, currentUser);
            assertEquals(ticket, updatedTicket);

            verify(validator, times(i)).beforeUpdate(any());
            verify(repository, times(i)).existsById(any());
            verify(repository, times(i)).findByIdAndAccountId(any(), any());
            verify(repository, times(i)).update(any());
            verify(counterService, times(0)).getTicketNumber(any());

            assertEquals(updatedTicket.getNumber(), ticketFromDataBase.getNumber());
            assertEquals(updatedTicket.getCreatedAt(), ticketFromDataBase.getCreatedAt());
            assertEquals(updatedTicket.getFiles(), ticketFromDataBase.getFiles());
            assertEquals(updatedTicket.getTicketTemplate(), ticketFromDataBase.getTicketTemplate());
            assertEquals(ticket.getGroup(), ticket.getAuthor().getGroup());

            assertEquals(updatedTicket.getObservers(), ticketFromDataBase.getObservers());
            assertEquals(updatedTicket.getExecutors(), ticketFromDataBase.getExecutors());
        }
    }

    @Test
    void update_shouldUpdateTicketAndSetFieldsFromDataBase_whenCurrentUserAuthorNotInnerGroup() {
        currentUser.getGroup().setIsInner(false);
        currentUser.setRole(roleTestHelper.setPredefinedValidEntityList().get(3));
        var ticketFromDataBase = ticketTestHelper.getRandomValidEntity();
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(repository.existsById(any())).thenReturn(true);
        when(repository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(ticketFromDataBase));
        when(repository.update(any())).thenReturn(ticket);

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

    @Test
    void update_shouldUpdateTicketAndNotSetFieldsFromDataBase_whenPassedValidDataCurrentUserAccountOwnerAdminExecutorInnerGroup() {
        currentUser.getGroup().setIsInner(true);
        var ticketFromDataBase = ticketTestHelper.getRandomValidEntity();
        when(validator.beforeUpdate(any())).thenReturn(true);
        when(repository.existsById(any())).thenReturn(true);
        when(repository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(ticketFromDataBase));
        when(repository.update(any())).thenReturn(ticket);
        for (int i = 0; i <= 2; i++) {
            currentUser.setRole(roleTestHelper.setPredefinedValidEntityList().get(i));
            var updatedTicket = service.update(ticket, currentUser);
            assertEquals(ticket, updatedTicket);

            verify(validator, times(i + 1)).beforeUpdate(any());
            verify(repository, times(i + 1)).existsById(any());
            verify(repository, times(i + 1)).findByIdAndAccountId(any(), any());
            verify(repository, times(i + 1)).update(any());
            verify(counterService, times(0)).getTicketNumber(any());

            assertEquals(updatedTicket.getNumber(), ticketFromDataBase.getNumber());
            assertEquals(updatedTicket.getCreatedAt(), ticketFromDataBase.getCreatedAt());
            assertEquals(updatedTicket.getFiles(), ticketFromDataBase.getFiles());
            assertEquals(updatedTicket.getTicketTemplate(), ticketFromDataBase.getTicketTemplate());
            assertEquals(ticket.getGroup(), ticket.getAuthor().getGroup());

            assertEquals(updatedTicket.getObservers(), updatedTicket.getObservers());
            assertEquals(updatedTicket.getExecutors(), updatedTicket.getExecutors());
        }
    }

}