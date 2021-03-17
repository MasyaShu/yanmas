package ru.itterminal.botdesk.tickets.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static ru.itterminal.botdesk.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;

import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers;
import org.springframework.security.web.FilterChainProxy;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;

import ru.itterminal.botdesk.commons.exception.RestExceptionHandler;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.tickets.model.TicketCounter;
import ru.itterminal.botdesk.tickets.service.impl.TicketCounterServiceImpl;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketCounterControllerV1.class, FilterChainProxy.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class TicketCounterControllerV1Test {

    @Autowired
    private TicketCounterControllerV1 controller;

    @Autowired
    FilterChainProxy springSecurityFilterChain;

    @Autowired
    UserDetailsService userDetailsService;

    @MockBean
    private TicketCounterServiceImpl service;

    private MockMvc mockMvc;

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
    private static final String API = "api/v1/ticket-counter";

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldUpdate_whenValidDataPassed() throws Exception {
        var newTicketCounter = TicketCounter.builder()
                .version(0)
                .id(UUID.randomUUID())
                .currentNumber(15L)
                .deleted(false)
                .build();
        when(service.update(any())).thenReturn(newTicketCounter);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(newTicketCounter));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.currentNumber").value(15L));
        verify(service, times(1)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void getById_shouldFindTicketCounter_whenAccordingWithPlannedBehavior() throws Exception {
        var foundTicketCounter = TicketCounter.builder()
                .version(0)
                .id(UUID.randomUUID())
                .currentNumber(15L)
                .deleted(false)
                .build();
        when(service.getTicketCounter(any())).thenReturn(foundTicketCounter);
        mockMvc.perform(get(HOST + PORT + API ))
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.currentNumber").value(15L));
        verify(service, times(1)).getTicketCounter(any());
    }
}