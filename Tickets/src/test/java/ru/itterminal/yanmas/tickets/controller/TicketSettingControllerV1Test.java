package ru.itterminal.yanmas.tickets.controller;

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
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;

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

import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.aau.util.ReflectionHelper;
import ru.itterminal.yanmas.commons.exception.RestExceptionHandler;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.commons.util.CommonConstants;
import ru.itterminal.yanmas.security.config.TestSecurityConfig;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;
import ru.itterminal.yanmas.tickets.model.TicketSetting;
import ru.itterminal.yanmas.tickets.model.dto.TicketSettingDtoRequest;
import ru.itterminal.yanmas.tickets.model.dto.TicketSettingDtoResponse;
import ru.itterminal.yanmas.tickets.model.test.TicketSettingTestHelper;
import ru.itterminal.yanmas.tickets.service.impl.TicketSettingServiceImpl;

@SuppressWarnings("unused")
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketSettingControllerV1.class, FilterChainProxy.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class TicketSettingControllerV1Test {

    @MockBean
    private UserServiceImpl userService;

    @MockBean
    private JwtUserBuilder jwtUserBuilder;

    @MockBean
    private ReflectionHelper reflectionHelper;

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
    private static final String API = "api/v1/ticket/setting-initial";
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
        requestDto = helper.convertEntityToDtoRequest(ticketSetting, true);
        requestDto.setDisplayName(null);
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
}
