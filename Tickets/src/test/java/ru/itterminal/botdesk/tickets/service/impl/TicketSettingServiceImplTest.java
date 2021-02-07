package ru.itterminal.botdesk.tickets.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.security.jwt.JwtUserBuilder;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.projection.TicketSettingUniqueFields;
import ru.itterminal.botdesk.tickets.model.test.TicketSettingTestHelper;
import ru.itterminal.botdesk.tickets.model.test.TicketStatusTestHelper;
import ru.itterminal.botdesk.tickets.model.test.TicketTypeTestHelper;
import ru.itterminal.botdesk.tickets.repository.TicketSettingRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketSettingOperationValidator;

@SpringJUnitConfig(value = {TicketSettingServiceImpl.class})
class TicketSettingServiceImplTest {

    @Autowired
    private TicketSettingServiceImpl service;

    @SuppressWarnings("unused")
    @MockBean
    private TicketSettingOperationValidator validator;

    @MockBean
    private TicketSettingRepository repository;

    @MockBean
    private TicketTypeServiceImpl ticketTypeService;

    @MockBean
    private TicketStatusServiceImpl ticketStatusService;

    @MockBean
    private JwtUserBuilder jwtUserBuilder;

    @MockBean
    private JwtUser jwtUser;

    @Mock
    TicketSettingUniqueFields ticketSettingUniqueFields;

    private final TicketSettingTestHelper helper = new TicketSettingTestHelper();
    private final TicketTypeTestHelper ticketTypeHelper = new TicketTypeTestHelper();
    private final TicketStatusTestHelper ticketStatusHelper = new TicketStatusTestHelper();

    @BeforeEach
    void setUp() {
        when(jwtUserBuilder.getJwtUser()).thenReturn(jwtUser);
        when(jwtUser.getAccountId()).thenReturn(UUID.randomUUID());
    }

    @Test
    void findByUniqueFields_shouldGetEmptyList_whenPassedDataIsUnique() {
        when(repository.findByUniqueFields(any(), any(), any(), any())).thenReturn(Collections.emptyList());
        TicketSetting ticketSetting = helper.getRandomValidEntity();
        List<TicketSettingUniqueFields> list = service.findByUniqueFields(ticketSetting);
        assertTrue(list.isEmpty());
        verify(repository, times(1)).findByUniqueFields(any(), any(), any(), any());
    }

    @Test
    void findByUniqueFields_shouldGetEmptyList_whenGroupAndAuthorIsNull() {
        when(repository.findByUniqueFields(any(), any(), any(), any())).thenReturn(Collections.emptyList());
        TicketSetting ticketSetting = helper.getRandomValidEntity();
        ticketSetting.setAuthor(null);
        ticketSetting.setGroup(null);
        List<TicketSettingUniqueFields> list = service.findByUniqueFields(ticketSetting);
        assertTrue(list.isEmpty());
        verify(repository, times(1)).findByUniqueFields(any(), any(), any(), any());
    }

    @Test
    void getSettingOrPredefinedValuesForTicket_shouldGetAllValueFromTicketSetting_whenAccordingWithPlannedBehavior() {
        when(ticketSettingUniqueFields.getId()).thenReturn(UUID.randomUUID());
        when(repository.findByUniqueFieldsWithoutDeleted(any(), any(), any(), any()))
                .thenReturn(List.of(ticketSettingUniqueFields));
        var expectedTicketSetting = helper.getPredefinedValidEntityList().get(0);
        when(repository.findById(any())).thenReturn(Optional.of(expectedTicketSetting));
        var someId = UUID.randomUUID();
        var actualTicketSetting = service.getSettingOrPredefinedValuesForTicket(someId, someId, someId);
        assertEquals(expectedTicketSetting, actualTicketSetting);
        verify(ticketSettingUniqueFields, times(1)).getId();
        verify(repository, times(1)).findByUniqueFieldsWithoutDeleted(any(), any(), any(), any());
        verify(repository, times(1)).findById(any());
        verify(ticketTypeService, times(0)).findByIdAndAccountId(any());
        verify(ticketStatusService, times(0)).findByIdAndAccountId(any());
    }

    @Test
    void getSettingOrPredefinedValuesForTicket_shouldGetAllValueFromPredefinedValues_whenAccordingWithPlannedBehavior() {
        when(repository.findByUniqueFieldsWithoutDeleted(any(), any(), any(), any()))
                .thenReturn(Collections.emptyList());
        var expectedTicketTypeForNew = ticketTypeHelper.getRandomValidEntity();
        var expectedTicketStatus = ticketStatusHelper.getRandomValidEntity();

        when(ticketTypeService.findStartedPredefinedTicketTypeForNewTicket(any())).thenReturn(expectedTicketTypeForNew);
        when(ticketStatusService.findStartedPredefinedStatus(any())).thenReturn(expectedTicketStatus);
        when(ticketStatusService.findReopenedPredefinedStatus(any())).thenReturn(expectedTicketStatus);
        when(ticketStatusService.findFinishedPredefinedStatus(any())).thenReturn(expectedTicketStatus);
        when(ticketStatusService.findCanceledPredefinedStatus(any())).thenReturn(expectedTicketStatus);

        var someId = UUID.randomUUID();
        var actualTicketSetting = service.getSettingOrPredefinedValuesForTicket(someId, someId, someId);

        assertEquals(expectedTicketTypeForNew, actualTicketSetting.getTicketTypeForNew());
        assertEquals(expectedTicketStatus, actualTicketSetting.getTicketStatusForNew());
        assertEquals(expectedTicketStatus, actualTicketSetting.getTicketStatusForReopen());
        assertEquals(expectedTicketStatus, actualTicketSetting.getTicketStatusForClose());
        assertEquals(expectedTicketStatus, actualTicketSetting.getTicketStatusForCancel());

        verify(ticketSettingUniqueFields, times(0)).getId();
        verify(repository, times(3)).findByUniqueFieldsWithoutDeleted(any(), any(), any(), any());
        verify(repository, times(0)).findById(any());
        verify(ticketTypeService, times(1)).findStartedPredefinedTicketTypeForNewTicket(any());
        verify(ticketStatusService, times(1)).findStartedPredefinedStatus(any());
        verify(ticketStatusService, times(1)).findReopenedPredefinedStatus(any());
        verify(ticketStatusService, times(1)).findFinishedPredefinedStatus(any());
        verify(ticketStatusService, times(1)).findCanceledPredefinedStatus(any());
    }

}