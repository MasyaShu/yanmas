package ru.itterminal.botdesk.tickets.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers;
import org.springframework.security.web.FilterChainProxy;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;

import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.RestExceptionHandler;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.tickets.model.dto.TicketSettingDtoRequest;
import ru.itterminal.botdesk.tickets.model.dto.TicketSettingDtoResponse;
import ru.itterminal.botdesk.tickets.model.spec.TicketSettingSpec;
import ru.itterminal.botdesk.tickets.model.test.TicketSettingTestHelper;
import ru.itterminal.botdesk.tickets.service.impl.TicketSettingServiceImpl;
import ru.itterminal.botdesk.tickets.service.impl.TicketStatusServiceImpl;
import ru.itterminal.botdesk.tickets.service.impl.TicketTypeServiceImpl;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketSettingControllerV1.class, TicketSettingSpec.class, FilterChainProxy.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles("Test")
class TicketSettingControllerV1Test {

    @MockBean
    private UserServiceImpl userService;

    @MockBean
    private AccountServiceImpl accountService;

    @MockBean
    private GroupServiceImpl groupService;

    @MockBean
    private TicketTypeServiceImpl ticketTypeService;

    @MockBean
    private TicketStatusServiceImpl ticketStatusService;

    @MockBean
    private TicketSettingServiceImpl ticketSettingService;

    @Autowired
    FilterChainProxy springSecurityFilterChain;

    @Autowired
    private TicketSettingControllerV1 controller;

    private MockMvc mockMvc;

    private final ModelMapper mapper = new ModelMapper();

    private final TicketSettingTestHelper helper = new TicketSettingTestHelper();

    @BeforeAll
    void setUpBeforeAll() {
        mockMvc = MockMvcBuilders.standaloneSetup(controller)
                .setControllerAdvice(new RestExceptionHandler())
                .apply(SecurityMockMvcConfigurers.springSecurity(springSecurityFilterChain))
                .build();
    }

    private final ObjectMapper objectMapper = new ObjectMapper();
    private static final String HOST = "http://localhost";
    private static final String PORT = ":8081";
    private static final String API = "api/v1/ticketSetting";

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldCreate_whenValidDataPassed() throws Exception {
        var ticketSetting = helper.getPredefinedValidEntityList().get(0);
        UUID accountId = ticketSetting.getAccount().getId();
        TicketSettingDtoRequest requestDto = helper.convertEntityToDtoRequest(ticketSetting);

        when(accountService.findById(any())).thenReturn(ticketSetting.getAccount());
        when(groupService.findByIdAndAccountId(any(), any())).thenReturn(ticketSetting.getGroup());
        when(userService.findByIdAndAccountId(any(), any())).thenReturn(ticketSetting.getAuthor());
        when(ticketTypeService.findByIdAndAccountId(any(), any())).thenReturn(ticketSetting.getTicketTypeForNew());
        when(ticketStatusService.findByIdAndAccountId(ticketSetting.getTicketStatusForNew().getId(), accountId)).
                thenReturn(ticketSetting.getTicketStatusForNew());
        when(ticketStatusService.findByIdAndAccountId(ticketSetting.getTicketStatusForReopen().getId(), accountId)).
                thenReturn(ticketSetting.getTicketStatusForReopen());
        when(ticketStatusService.findByIdAndAccountId(ticketSetting.getTicketStatusForClose().getId(), accountId)).
                thenReturn(ticketSetting.getTicketStatusForClose());
        when(ticketStatusService.findByIdAndAccountId(ticketSetting.getTicketStatusForCancel().getId(), accountId)).
                thenReturn(ticketSetting.getTicketStatusForCancel());
        when(ticketSettingService.create(any())).thenReturn(ticketSetting);

        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(requestDto));

        var requestResult = mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isCreated())
                .andReturn()
                .getResponse()
                .getContentAsString();

        TicketSettingDtoResponse actualTicketSettingDtoResponse =
                objectMapper.readValue(requestResult, TicketSettingDtoResponse.class);

        TicketSettingDtoResponse expectedTicketSettingDtoResponse =
                mapper.map(
                        ticketSetting,
                        TicketSettingDtoResponse.class
                );

        assertEquals(expectedTicketSettingDtoResponse, actualTicketSettingDtoResponse);

        verify(accountService, times(1)).findById(any());
        verify(groupService, times(1)).findByIdAndAccountId(any(), any());
        verify(userService, times(1)).findByIdAndAccountId(any(), any());
        verify(ticketTypeService, times(1)).findByIdAndAccountId(any(), any());
        verify(ticketStatusService, times(4)).findByIdAndAccountId(any(), any());
    }

}