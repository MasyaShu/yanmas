package ru.itterminal.botdesk.tickets.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static ru.itterminal.botdesk.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;

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
import org.springframework.security.test.context.support.WithAnonymousUser;
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

import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.RestExceptionHandler;
import ru.itterminal.botdesk.commons.model.spec.SpecificationsFactory;
import ru.itterminal.botdesk.commons.util.CommonConstants;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.dto.TicketSettingDtoRequest;
import ru.itterminal.botdesk.tickets.model.dto.TicketSettingDtoResponse;
import ru.itterminal.botdesk.tickets.model.test.TicketSettingTestHelper;
import ru.itterminal.botdesk.tickets.service.impl.TicketSettingServiceImpl;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketSettingControllerV1.class, FilterChainProxy.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class TicketSettingControllerV1Test {

    @MockBean
    private UserServiceImpl userService;

    @SuppressWarnings("unused")
    @MockBean
    private SpecificationsFactory specFactory;

    @MockBean
    private TicketSettingServiceImpl ticketSettingService;

    @Autowired
    FilterChainProxy springSecurityFilterChain;

    @Autowired
    private TicketSettingControllerV1 controller;

    private final ObjectMapper objectMapper = new ObjectMapper();
    private static final String HOST = "http://localhost";
    private static final String PORT = ":8081";
    private static final String API = "api/v1/ticket-setting";
    private final TicketSettingTestHelper helper = new TicketSettingTestHelper();
    private final ModelMapper mapper = new ModelMapper();

    private MockMvc mockMvc;
    private TicketSetting ticketSetting;
    TicketSettingDtoRequest requestDto;

    @BeforeAll
    void setUpBeforeAll() {
        mockMvc = MockMvcBuilders.standaloneSetup(controller)
                .setControllerAdvice(new RestExceptionHandler())
                .apply(SecurityMockMvcConfigurers.springSecurity(springSecurityFilterChain))
                .build();
        ticketSetting = helper.getPredefinedValidEntityList().get(0);
    }

    @BeforeEach
    void setupBeforeEach() {
        requestDto = helper.convertEntityToDtoRequest(ticketSetting);
        requestDto.setDisplayName(null);
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldCreate_whenValidDataPassed() throws Exception {
        requestDto.setId(null);
        requestDto.setDeleted(null);
        requestDto.setVersion(null);
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
        verify(ticketSettingService, times(1)).create(any());
    }

    @Test
    @WithAnonymousUser
    void create_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(requestDto));

        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isUnauthorized());
        verify(userService, times(0)).findByIdAndAccountId(any());
        verify(ticketSettingService, times(0)).create(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusForbidden_whenNotAllowedRole() throws Exception {
        requestDto.setId(null);
        requestDto.setDeleted(null);
        requestDto.setVersion(null);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(requestDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(userService, times(0)).findByIdAndAccountId(any());
        verify(ticketSettingService, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenVersionNotNull() throws Exception {
        TicketSettingDtoRequest dtoRequest = helper.convertEntityToDtoRequest(helper.getRandomValidEntity());
        dtoRequest.setVersion(15);
        dtoRequest.setId(UUID.randomUUID());
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(dtoRequest));

        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.id[?(@.message == '%s')]",
                                           CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY
                                   ).exists())
                .andExpect(
                        MockMvcResultMatchers
                                .jsonPath(
                                        "$.errors.version[?(@.message == '%s')]",
                                        CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY
                                ).exists());
        verify(userService, times(0)).findByIdAndAccountId(any());
        verify(ticketSettingService, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldUpdate_whenValidDataPassed() throws Exception {
        requestDto.setDeleted(false);
        when(ticketSettingService.update(any())).thenReturn(ticketSetting);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(requestDto));
        var requestResult = mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
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
        verify(ticketSettingService, times(1)).update(any());
    }

    @Test
    @WithAnonymousUser
    void update_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(requestDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isUnauthorized());
        verify(userService, times(0)).findByIdAndAccountId(any());
        verify(ticketSettingService, times(0)).update(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusForbidden_whenNotAllowedRole() throws Exception {
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(requestDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(userService, times(0)).findByIdAndAccountId(any());
        verify(ticketSettingService, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidIdPassed() throws Exception {
        UUID uuid = UUID.randomUUID();
        requestDto.setId(uuid);
        requestDto.setVersion(1);
        String json = objectMapper.writeValueAsString(requestDto);
        json = json.replace(uuid.toString(), "abracadabra");
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(json);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.title").value(CommonConstants.MESSAGE_NOT_READABLE));
        verify(userService, times(0)).findByIdAndAccountId(any());
        verify(ticketSettingService, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getSettingOrPredefinedValuesForTicket_shouldGetTicketSetting_whenPassedValidAuthorId() throws Exception {
        when(userService.findByIdAndAccountId(any())).thenReturn(ticketSetting.getAuthor());
        when(ticketSettingService.getSettingOrPredefinedValuesForTicket(any(), any(), any())).thenReturn(ticketSetting);

        TicketSettingDtoResponse expectedTicketSettingDtoResponse =
                mapper.map(
                        ticketSetting,
                        TicketSettingDtoResponse.class
                );

        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + "/by-author/" + ticketSetting.getAuthor().getId())
                        .contentType(MediaType.APPLICATION_JSON)
                        .accept(MediaType.APPLICATION_JSON);

        var requestResult = mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andReturn()
                .getResponse()
                .getContentAsString();

        TicketSettingDtoResponse actualTicketSettingDtoResponse =
                objectMapper.readValue(requestResult, TicketSettingDtoResponse.class);

        assertEquals(expectedTicketSettingDtoResponse, actualTicketSettingDtoResponse);

        verify(userService, times(1)).findByIdAndAccountId(any());
        verify(ticketSettingService, times(1)).getSettingOrPredefinedValuesForTicket(any(), any(), any());
    }

    @Test
    @WithUserDetails("OBSERVER_ACCOUNT_1_IS_INNER_GROUP")
    void getSettingOrPredefinedValuesForTicket_shouldGetStatusForbidden_whenUserHasRoleObserver() throws Exception {
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + "/by-author/" + ticketSetting.getAuthor().getId())
                        .contentType(MediaType.APPLICATION_JSON)
                        .accept(MediaType.APPLICATION_JSON);

        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(userService, times(0)).findByIdAndAccountId(any());
        verify(ticketSettingService, times(0)).getSettingOrPredefinedValuesForTicket(any(), any(), any());
    }

    @Test
    @WithUserDetails("OBSERVER_ACCOUNT_1_IS_INNER_GROUP")
    void getById_shouldGetStatusForbidden_whenUserHasRoleObserver() throws Exception {
        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + "/" + ticketSetting.getAuthor().getId())
                        .contentType(MediaType.APPLICATION_JSON)
                        .accept(MediaType.APPLICATION_JSON);

        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(ticketSettingService, times(0)).findByIdAndAccountId(any());
    }
}