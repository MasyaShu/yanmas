package ru.itterminal.botdesk.tickets.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
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
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.commons.exception.RestExceptionHandler;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.tickets.model.dto.TicketTemplateDtoRequest;
import ru.itterminal.botdesk.tickets.model.test.TicketTemplateTestHelper;
import ru.itterminal.botdesk.tickets.service.impl.TicketTemplateServiceImpl;

import java.util.UUID;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;


@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketTemplateControllerV1.class, FilterChainProxy.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles("Test")
class TicketTemplateControllerV1Test {

    @MockBean
    private AccountServiceImpl accountService;

    @MockBean
    TicketTemplateServiceImpl templateService;

    @Autowired
    private TicketTemplateControllerV1 controller;

    @Autowired
    FilterChainProxy springSecurityFilterChain;

    private final TicketTemplateTestHelper ticketTemplateTestHelper = new TicketTemplateTestHelper();


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
    private static final String API = "api/v1/ticketTemplate/";
    private TicketTemplateDtoRequest ticketTemplateDtoRequest;


    @BeforeEach
    void setUpBeforeEach() {
        ticketTemplateDtoRequest = TicketTemplateDtoRequest
                .builder()
                .subject("111")
                .zoneId("Etc/GMT+2")
                .expressionSchedule("1 1 1 28 2 1")
                .isActive(true)
                .ticketTypeId(UUID.fromString("7f66b241-f8ec-4912-8f58-a4ceef2dd4c9"))
                .authorId(UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de"))
                .isOnlyOneTicketInWork(true)
                .build();
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldCreate_whenValidDataPassed() throws Exception {
        ticketTemplateDtoRequest.setDeleted(null);
        when(templateService.create(any())).thenReturn(ticketTemplateTestHelper.getRandomValidEntity());
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketTemplateDtoRequest));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isCreated());
    }
}