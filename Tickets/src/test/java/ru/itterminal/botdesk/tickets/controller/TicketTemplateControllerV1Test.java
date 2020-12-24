package ru.itterminal.botdesk.tickets.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
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
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.commons.exception.RestExceptionHandler;
import ru.itterminal.botdesk.commons.util.CommonConstants;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;
import ru.itterminal.botdesk.tickets.model.dto.TicketTemplateDtoRequest;
import ru.itterminal.botdesk.tickets.model.dto.TicketTemplateDtoResponse;
import ru.itterminal.botdesk.tickets.model.test.TicketTemplateTestHelper;
import ru.itterminal.botdesk.tickets.service.impl.TicketTemplateServiceImpl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;


@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketTemplateControllerV1.class, FilterChainProxy.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles("Test")
class TicketTemplateControllerV1Test {

    private static final String SCHEDULER = "Scheduler";
    private static final String ZONE_ID = "ZoneId";
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
    private final TicketTemplateTestHelper templateTestHelper = new TicketTemplateTestHelper();
    private final ModelMapper mapper = new ModelMapper();


    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldCreate_whenValidDataPassed() throws Exception {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        TicketTemplateDtoRequest ticketTemplateDtoRequest = ticketTemplateTestHelper.convertEntityToDtoRequest(ticketTemplate);
        ticketTemplateDtoRequest.setVersion(null);
        ticketTemplateDtoRequest.setId(null);
        ticketTemplateDtoRequest.setDeleted(null);
        when(templateService.create(any())).thenReturn(ticketTemplate);
        when(accountService.findById(any())).thenReturn(ticketTemplate.getAccount());
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketTemplateDtoRequest));

        var requestResult = mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isCreated())
                .andReturn()
                .getResponse()
                .getContentAsString();

        TicketTemplateDtoResponse actualTicketTemplateDtoResponse =
                objectMapper.readValue(requestResult, TicketTemplateDtoResponse.class);

        TicketTemplateDtoResponse expectedTicketTemplateDtoResponse =
                mapper.map(ticketTemplate, TicketTemplateDtoResponse.class);

        assertEquals(expectedTicketTemplateDtoResponse, actualTicketTemplateDtoResponse);

        verify(accountService, times(1)).findById(any());
        verify(templateService, times(1)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldBadRequestWithErrorsDescriptions_whenValidDataInvalid() throws Exception {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        TicketTemplateDtoRequest ticketTemplateDtoRequest = ticketTemplateTestHelper.convertEntityToDtoRequest(ticketTemplate);
        ticketTemplateDtoRequest.setZoneId(null);
        ticketTemplateDtoRequest.setExpressionSchedule(null);
        ticketTemplateDtoRequest.setAuthorId(null);
        ticketTemplateDtoRequest.setTicketTypeId(null);
        ticketTemplateDtoRequest.setIsActive(null);
        ticketTemplateDtoRequest.setIsOnlyOneTicketInWork(null);
        when(templateService.create(any())).thenReturn(ticketTemplate);
        when(accountService.findById(any())).thenReturn(ticketTemplate.getAccount());
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketTemplateDtoRequest));

        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.expressionSchedule[?(@.code == '%s')]", SCHEDULER).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.zoneId[?(@.code == '%s')]", ZONE_ID).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.id[?(@.message == '%s')]", CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.version[?(@.message == '%s')]", CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.deleted[?(@.message == '%s')]", CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.authorId[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.ticketTypeId[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.isActive[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.isOnlyOneTicketInWork[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists());

        verify(accountService, times(0)).findById(any());
        verify(templateService, times(0)).create(any());
    }
}