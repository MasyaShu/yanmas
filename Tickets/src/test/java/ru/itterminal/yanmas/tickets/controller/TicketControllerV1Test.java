package ru.itterminal.yanmas.tickets.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;

import java.util.Collections;
import java.util.Locale;

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
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.javafaker.Faker;

import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.WhoWatchedEntityServiceImpl;
import ru.itterminal.yanmas.aau.util.ReflectionHelper;
import ru.itterminal.yanmas.commons.exception.RestExceptionHandler;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.commons.util.CommonConstants;
import ru.itterminal.yanmas.security.config.TestSecurityConfig;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.model.dto.TicketDtoRequest;
import ru.itterminal.yanmas.tickets.model.dto.TicketDtoResponse;
import ru.itterminal.yanmas.tickets.model.test.TicketTestHelper;
import ru.itterminal.yanmas.tickets.service.impl.TicketServiceImpl;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketControllerV1.class, FilterChainProxy.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class TicketControllerV1Test {

    public static final String MUST_BE_ANY_OF_LOW_MIDDLE_HEIGHT = "must be any of: low, middle, height";
    public static final String ERROR_PRIORITY = "Error priority";
    @MockBean
    private UserServiceImpl userService;

    @SuppressWarnings("unused")
    @MockBean
    private ReflectionHelper reflectionHelper;

    @MockBean
    private TicketServiceImpl ticketService;

    @SuppressWarnings("unused")
    @MockBean
    private SpecificationsFactory specFactory;

    @SuppressWarnings("unused")
    @MockBean
    private WhoWatchedEntityServiceImpl whoWatchedEntityService;

    @Autowired
    FilterChainProxy springSecurityFilterChain;

    @Autowired
    private TicketControllerV1 controller;

    private static final String HOST = "http://localhost";
    private static final String PORT = ":8081";
    private static final String API = "api/v1/ticket";
    private final TicketTestHelper ticketTestHelper = new TicketTestHelper();
    private final ModelMapper mapper = new ModelMapper();
    private final ObjectMapper objectMapper = new ObjectMapper();

    private MockMvc mockMvc;
    private Ticket ticket;
    private TicketDtoRequest requestDto;
    private final Faker fakerRU = new Faker(new Locale("ru", "RU"));

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
        ticket = ticketTestHelper.getRandomValidEntity();
    }

    @BeforeEach
    void setupBeforeEach() {
        requestDto = ticketTestHelper.convertEntityToDtoRequest(ticket, true);
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldCreate_whenValidDataPassed() throws Exception {
        requestDto.setId(null);
        requestDto.setDeleted(null);
        requestDto.setVersion(null);
        requestDto.setDisplayName(null);
        when(userService.findByEmail(any())).thenReturn(ticket.getAuthor());
        if (requestDto.getObservers() != null && !requestDto.getObservers().isEmpty()) {
            when(userService.findAllByAccountIdAndListId(requestDto.getObservers()))
                    .thenReturn(ticket.getObservers());
        } else {
            when(userService.findAllByAccountIdAndListId(Collections.emptyList())).thenReturn(null);
        }
        if (requestDto.getExecutors() != null && !requestDto.getExecutors().isEmpty()) {
            when(userService.findAllByAccountIdAndListId(requestDto.getExecutors()))
                    .thenReturn(ticket.getExecutors());
        }
        when(ticketService.create(any(), any())).thenReturn(ticket);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(requestDto));

        var response = mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isCreated())
                .andReturn()
                .getResponse()
                .getContentAsString();
        var actualTicketDtoResponse = objectMapper.readValue(response, TicketDtoResponse.class);
        var expectedTicketDtoResponse = mapper.map(ticket, TicketDtoResponse.class);
        assertEquals(expectedTicketDtoResponse, actualTicketDtoResponse);
        verify(userService, times(0)).findByIdAndAccountId(any());
        verify(userService, times(0)).findAllByAccountIdAndListId(any());
        verify(userService, times(1)).findByEmail(any());
        verify(ticketService, times(1)).create(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenPassedDataIsInvalid() throws Exception {
        requestDto.setId(null);
        requestDto.setDeleted(null);
        requestDto.setVersion(null);
        requestDto.setDisplayName(null);
        requestDto.setAuthorId(null);
        requestDto.setPriority("");
        requestDto.setSubject(fakerRU.lorem().sentence(256));
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(requestDto));

        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.authorId[?(@.message == '%s')]",
                                           CommonConstants.MUST_NOT_BE_NULL
                                   ).exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.priority[?(@.message == '%s')]",
                                           MUST_BE_ANY_OF_LOW_MIDDLE_HEIGHT
                                   ).exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.subject[?(@.message =~ /%s.*/)]",
                                           CommonConstants.SIZE_MUST_BE_BETWEEN
                                   ).exists());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldUpdate_whenValidDataPassed() throws Exception {
        requestDto.setDisplayName(null);
        requestDto.setFiles(null);
        when(userService.findByEmail(any())).thenReturn(ticket.getAuthor());
        if (requestDto.getObservers() != null && !requestDto.getObservers().isEmpty()) {
            when(userService.findAllByAccountIdAndListId(requestDto.getObservers()))
                    .thenReturn(ticket.getObservers());
        } else {
            when(userService.findAllByAccountIdAndListId(Collections.emptyList())).thenReturn(null);
        }
        if (requestDto.getExecutors() != null && !requestDto.getExecutors().isEmpty()) {
            when(userService.findAllByAccountIdAndListId(requestDto.getExecutors()))
                    .thenReturn(ticket.getExecutors());
        }
        when(ticketService.update(any(), any())).thenReturn(ticket);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(requestDto));

        var response = mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andReturn()
                .getResponse()
                .getContentAsString();
        var actualTicketDtoResponse = objectMapper.readValue(response, TicketDtoResponse.class);
        var expectedTicketDtoResponse = mapper.map(ticket, TicketDtoResponse.class);
        assertEquals(expectedTicketDtoResponse, actualTicketDtoResponse);
        verify(userService, times(0)).findByIdAndAccountId(any());
        verify(userService, times(0)).findAllByAccountIdAndListId(any());
        verify(userService, times(1)).findByEmail(any());
        verify(ticketService, times(1)).update(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        requestDto.setId(null);
        requestDto.setVersion(null);
        requestDto.setDeleted(null);
        requestDto.setAuthorId(null);
        requestDto.setPriority(ERROR_PRIORITY);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(requestDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.id[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.version[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.files[?(@.message == '%s')]", CommonConstants.MUST_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.displayName[?(@.message == '%s')]", CommonConstants.MUST_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.deleted[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.authorId[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.priority[?(@.message == '%s')]",
                                           MUST_BE_ANY_OF_LOW_MIDDLE_HEIGHT
                                   )
                                   .exists());
        verify(userService, times(0)).findByIdAndAccountId(any());
        verify(userService, times(0)).findAllByAccountIdAndListId(any());
        verify(userService, times(0)).findByEmail(any());
        verify(ticketService, times(0)).update(any(), any());
    }
}
