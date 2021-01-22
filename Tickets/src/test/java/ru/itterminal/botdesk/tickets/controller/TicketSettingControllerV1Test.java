package ru.itterminal.botdesk.tickets.controller;

import static org.hamcrest.Matchers.hasSize;
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

import java.util.List;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
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

import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.exception.RestExceptionHandler;
import ru.itterminal.botdesk.commons.model.spec.SpecificationsFactory;
import ru.itterminal.botdesk.commons.util.CommonConstants;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.dto.TicketSettingDtoRequest;
import ru.itterminal.botdesk.tickets.model.dto.TicketSettingDtoResponse;
import ru.itterminal.botdesk.tickets.model.dto.TicketSettingFilterDto;
import ru.itterminal.botdesk.tickets.model.test.TicketSettingTestHelper;
import ru.itterminal.botdesk.tickets.service.impl.TicketSettingServiceImpl;
import ru.itterminal.botdesk.tickets.service.impl.TicketStatusServiceImpl;
import ru.itterminal.botdesk.tickets.service.impl.TicketTypeServiceImpl;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketSettingControllerV1.class, FilterChainProxy.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles("Test")
class TicketSettingControllerV1Test {

    public static final String DISPLAY_NAME = "displayName";
    @MockBean
    private UserServiceImpl userService;

    @MockBean
    private SpecificationsFactory specFactory;

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
        UUID accountId = ticketSetting.getAccount().getId();
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
                .andExpect(status().isForbidden());
        verify(accountService, times(0)).findById(any());
        verify(groupService, times(0)).findByIdAndAccountId(any(), any());
        verify(userService, times(0)).findByIdAndAccountId(any(), any());
        verify(ticketTypeService, times(0)).findByIdAndAccountId(any(), any());
        verify(ticketStatusService, times(0)).findByIdAndAccountId(any(), any());
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
        verify(accountService, times(0)).findById(any());
        verify(groupService, times(0)).findByIdAndAccountId(any(), any());
        verify(userService, times(0)).findByIdAndAccountId(any(), any());
        verify(ticketTypeService, times(0)).findByIdAndAccountId(any(), any());
        verify(ticketStatusService, times(0)).findByIdAndAccountId(any(), any());
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
        verify(accountService, times(0)).findById(any());
        verify(groupService, times(0)).findByIdAndAccountId(any(), any());
        verify(userService, times(0)).findByIdAndAccountId(any(), any());
        verify(ticketTypeService, times(0)).findByIdAndAccountId(any(), any());
        verify(ticketStatusService, times(0)).findByIdAndAccountId(any(), any());
        verify(ticketSettingService, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldUpdate_whenValidDataPassed() throws Exception {
        UUID accountId = ticketSetting.getAccount().getId();
        requestDto.setDeleted(false);
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

        verify(accountService, times(1)).findById(any());
        verify(groupService, times(1)).findByIdAndAccountId(any(), any());
        verify(userService, times(1)).findByIdAndAccountId(any(), any());
        verify(ticketTypeService, times(1)).findByIdAndAccountId(any(), any());
        verify(ticketStatusService, times(4)).findByIdAndAccountId(any(), any());
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
                .andExpect(status().isForbidden());
        verify(accountService, times(0)).findById(any());
        verify(groupService, times(0)).findByIdAndAccountId(any(), any());
        verify(userService, times(0)).findByIdAndAccountId(any(), any());
        verify(ticketTypeService, times(0)).findByIdAndAccountId(any(), any());
        verify(ticketStatusService, times(0)).findByIdAndAccountId(any(), any());
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
        verify(accountService, times(0)).findById(any());
        verify(groupService, times(0)).findByIdAndAccountId(any(), any());
        verify(userService, times(0)).findByIdAndAccountId(any(), any());
        verify(ticketTypeService, times(0)).findByIdAndAccountId(any(), any());
        verify(ticketStatusService, times(0)).findByIdAndAccountId(any(), any());
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
        verify(accountService, times(0)).findById(any());
        verify(groupService, times(0)).findByIdAndAccountId(any(), any());
        verify(userService, times(0)).findByIdAndAccountId(any(), any());
        verify(ticketTypeService, times(0)).findByIdAndAccountId(any(), any());
        verify(ticketStatusService, times(0)).findByIdAndAccountId(any(), any());
        verify(ticketSettingService, times(0)).update(any());
    }

    @Test
    @WithAnonymousUser
    void getByFilter_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(new TicketSettingFilterDto()));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(ticketSettingService, times(0)).findAllByFilter(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getByFilter_shouldFindAll_whenFilterIsNew() throws Exception {
        Pageable pageable =
                PageRequest.of(Integer.parseInt(BaseController.PAGE_DEFAULT_VALUE), Integer.parseInt(
                        BaseController.SIZE_DEFAULT_VALUE),
                               Sort.by(DISPLAY_NAME).ascending()
                );
        Page<TicketSetting> ticketSettingPageExpected = new PageImpl<>(List.of(ticketSetting), pageable, 1);
        when(ticketSettingService.findAllByFilter(any(), any())).thenReturn(ticketSettingPageExpected);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(new TicketSettingFilterDto()));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].id").value(ticketSetting.getId().toString()))
                .andExpect(jsonPath("$.content", hasSize(1)));
        verify(ticketSettingService, times(1)).findAllByFilter(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getByFilter_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidSizeAndPagePassed()
            throws Exception {
        MockHttpServletRequestBuilder request = get(HOST + PORT + API + "?page=-1&size=0")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(new TicketSettingFilterDto()));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.title").value(CommonConstants.REQUEST_NOT_READABLE));
        verify(ticketSettingService, times(0)).findAllByFilter(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getByAuthorId_shouldGetTicketSetting_whenPassedValidAuthorId() throws Exception {
        when(userService.findByIdAndAccountId(any(), any())).thenReturn(ticketSetting.getAuthor());
        when(ticketSettingService.getSettingOrPredefinedValuesForTicket(any(), any(), any())).thenReturn(ticketSetting);

        TicketSettingDtoResponse expectedTicketSettingDtoResponse =
                mapper.map(
                        ticketSetting,
                        TicketSettingDtoResponse.class
                );

        MockHttpServletRequestBuilder request =
                get(HOST + PORT + API + "/" + ticketSetting.getAuthor().getId())
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

        verify(userService, times(1)).findByIdAndAccountId(any(), any());
        verify(ticketSettingService, times(1)).getSettingOrPredefinedValuesForTicket(any(), any(), any());
    }
}