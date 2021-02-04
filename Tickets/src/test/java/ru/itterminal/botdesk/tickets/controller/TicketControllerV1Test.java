package ru.itterminal.botdesk.tickets.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Collections;
import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
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
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.RestExceptionHandler;
import ru.itterminal.botdesk.commons.model.spec.SpecificationsFactory;
import ru.itterminal.botdesk.files.service.FileServiceImpl;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.tickets.model.Ticket;
import ru.itterminal.botdesk.tickets.model.dto.TicketDtoRequest;
import ru.itterminal.botdesk.tickets.model.dto.TicketDtoResponse;
import ru.itterminal.botdesk.tickets.model.test.TicketTestHelper;
import ru.itterminal.botdesk.tickets.service.impl.TicketServiceImpl;
import ru.itterminal.botdesk.tickets.service.impl.TicketSettingServiceImpl;
import ru.itterminal.botdesk.tickets.service.impl.TicketStatusServiceImpl;
import ru.itterminal.botdesk.tickets.service.impl.TicketTemplateServiceImpl;
import ru.itterminal.botdesk.tickets.service.impl.TicketTypeServiceImpl;

@SuppressWarnings("unused")
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketControllerV1.class, FilterChainProxy.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles("Test")
class TicketControllerV1Test {

    @MockBean
    private UserServiceImpl userService;

    @MockBean
    private TicketServiceImpl ticketService;

    @MockBean
    private AccountServiceImpl accountService;

    @MockBean
    private TicketTemplateServiceImpl ticketTemplateService;

    @MockBean
    private TicketTypeServiceImpl ticketTypeService;

    @MockBean
    private TicketStatusServiceImpl ticketStatusService;

    @MockBean
    private FileServiceImpl fileService;

    @MockBean
    private TicketSettingServiceImpl ticketSettingService;

    @MockBean
    private SpecificationsFactory specFactory;

    @Autowired
    FilterChainProxy springSecurityFilterChain;

    @Autowired
    private TicketControllerV1 controller;

    private static final String HOST = "http://localhost";
    private static final String PORT = ":8081";
    private static final String API = "api/v1/ticket";
    private final TicketTestHelper helper = new TicketTestHelper();
    private final ModelMapper mapper = new ModelMapper();
    private final ObjectMapper objectMapper = new ObjectMapper();

    private MockMvc mockMvc;
    private Ticket ticket;
    private TicketDtoRequest requestDto;

    @BeforeAll
    void setUpBeforeAll() {
        mockMvc = MockMvcBuilders.standaloneSetup(controller)
                .setControllerAdvice(new RestExceptionHandler())
                .apply(SecurityMockMvcConfigurers.springSecurity(springSecurityFilterChain))
                .addFilter((request, response, chain) -> {
                    response.setCharacterEncoding("UTF-8"); // this is crucial
                    chain.doFilter(request, response);
                }, "/*")
                .build();
        ticket = helper.getRandomValidEntity();
    }

    @BeforeEach
    void setupBeforeEach() {
        requestDto = helper.convertEntityToDtoRequest(ticket);
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldCreate_whenValidDataPassed() throws Exception {
        requestDto.setId(null);
        requestDto.setDeleted(null);
        requestDto.setVersion(null);
        requestDto.setDisplayName(null);
        UUID accountId = ticket.getAccount().getId();
        when(accountService.findById(any())).thenReturn(ticket.getAccount());
        when(userService.findByIdAndAccountId(any(), any())).thenReturn(ticket.getAuthor());
        when(userService.findByEmail(any())).thenReturn(ticket.getAuthor());
        when(ticketTypeService.findByIdAndAccountId(any(), any())).thenReturn(ticket.getTicketType());
        when(ticketStatusService.findByIdAndAccountId(any(), any())).thenReturn(ticket.getTicketStatus());
        when(ticketTemplateService.findByIdAndAccountId(any(), any())).thenReturn(ticket.getTicketTemplate());
        if (requestDto.getObservers() != null && !requestDto.getObservers().isEmpty()) {
            when(userService.findAllByAccountIdAndListId(accountId, requestDto.getObservers()))
                    .thenReturn(ticket.getObservers());
        } else {
            when(userService.findAllByAccountIdAndListId(accountId, Collections.emptyList())).thenReturn(null);
        }
        if (requestDto.getExecutors() != null && !requestDto.getExecutors().isEmpty()) {
            when(userService.findAllByAccountIdAndListId(accountId, requestDto.getExecutors()))
                    .thenReturn(ticket.getExecutors());
        } else {
            when(userService.findAllByAccountIdAndListId(accountId, Collections.emptyList())).thenReturn(null);
        }
        if (requestDto.getFiles() != null && !requestDto.getFiles().isEmpty()) {
            when(fileService.findAllByAccountIdAndListId(accountId, requestDto.getFiles()))
                    .thenReturn(ticket.getFiles());
        } else {
            when(fileService.findAllByAccountIdAndListId(accountId, Collections.emptyList())).thenReturn(null);
        }
        when(ticketService.create(any(), any())).thenReturn(ticket);
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

        var actualTicketDtoResponse = objectMapper.readValue(requestResult, TicketDtoResponse.class);
        var expectedTicketDtoResponse = mapper.map(ticket, TicketDtoResponse.class);

        assertEquals(expectedTicketDtoResponse, actualTicketDtoResponse);

        verify(accountService, times(1)).findById(any());
        verify(userService, times(1)).findByIdAndAccountId(any(), any());
        verify(userService, times(1)).findByEmail(any());
        verify(ticketSettingService, times(1)).getSettingOrPredefinedValuesForTicket(any(), any(), any());
        verify(ticketTypeService, times(1)).findByIdAndAccountId(any(), any());
        verify(ticketStatusService, times(1)).findByIdAndAccountId(any(), any());
        verify(ticketTemplateService, times(1)).findByIdAndAccountId(any(), any());
        if (requestDto.getObservers() != null && !requestDto.getObservers().isEmpty()) {
            verify(userService, times(1)).findAllByAccountIdAndListId(accountId, requestDto.getObservers());
        }
        if (requestDto.getExecutors() != null && !requestDto.getExecutors().isEmpty()) {
            verify(userService, times(1)).findAllByAccountIdAndListId(accountId, requestDto.getExecutors());
        }
        if (requestDto.getFiles() != null && !requestDto.getFiles().isEmpty()) {
            verify(fileService, times(1)).findAllByAccountIdAndListId(accountId, requestDto.getFiles());
        } else {
            verify(fileService, times(1)).findAllByAccountIdAndListId(accountId, Collections.emptyList());
        }
        verify(ticketService, times(1)).create(any(), any());
    }

}