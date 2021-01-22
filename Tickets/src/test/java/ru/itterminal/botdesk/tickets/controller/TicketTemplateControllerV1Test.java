package ru.itterminal.botdesk.tickets.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.web.servlet.HttpEncodingAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.PageImpl;
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
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.exception.RestExceptionHandler;
import ru.itterminal.botdesk.commons.model.filter.NumberFilter;
import ru.itterminal.botdesk.commons.model.filter.StringFilter;
import ru.itterminal.botdesk.commons.model.spec.SpecificationsFactory;
import ru.itterminal.botdesk.commons.util.CommonConstants;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;
import ru.itterminal.botdesk.tickets.model.dto.TicketTemplateDtoRequest;
import ru.itterminal.botdesk.tickets.model.dto.TicketTemplateDtoResponse;
import ru.itterminal.botdesk.tickets.model.dto.TicketTemplateFilterDto;
import ru.itterminal.botdesk.tickets.model.test.TicketTemplateTestHelper;
import ru.itterminal.botdesk.tickets.service.impl.TicketTemplateServiceImpl;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static ru.itterminal.botdesk.commons.model.validator.ZoneId.ZONE_ID_NOT_VALID;


@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketTemplateControllerV1.class, FilterChainProxy.class})
@Import({TestSecurityConfig.class, HttpEncodingAutoConfiguration.class})
@WebMvcTest
@ActiveProfiles("Test")
class TicketTemplateControllerV1Test {

    public static final String INVALID_VALUE_FOR = "Invalid value for";
    private static final String SCHEDULER = "Scheduler";
    private static final String ZONE_ID = "ZoneId";
    public static final String EXPRESSION_MUST_CONSIST = "Cron expression must consist";
    @MockBean
    private AccountServiceImpl accountService;

    @MockBean
    TicketTemplateServiceImpl templateService;

    @MockBean
    private SpecificationsFactory specFactory;

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
    private static final String API = "api/v1/ticket-template/";
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
    void create_shouldBadRequestWithErrorsDescriptions_whenInvalidData() throws Exception {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        TicketTemplateDtoRequest ticketTemplateDtoRequest = ticketTemplateTestHelper.convertEntityToDtoRequest(ticketTemplate);
        ticketTemplateDtoRequest.setZoneId(null);
        ticketTemplateDtoRequest.setExpressionSchedule(null);
        ticketTemplateDtoRequest.setAuthorId(null);
        ticketTemplateDtoRequest.setSubject(null);
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
                        .jsonPath("$.errors.subject[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.isActive[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.isOnlyOneTicketInWork[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists());

        verify(accountService, times(0)).findById(any());
        verify(templateService, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldBadRequestWithErrorsDescriptions_whenInvalidZoneId() throws Exception {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        TicketTemplateDtoRequest ticketTemplateDtoRequest = ticketTemplateTestHelper.convertEntityToDtoRequest(ticketTemplate);
        ticketTemplateDtoRequest.setId(null);
        ticketTemplateDtoRequest.setVersion(null);
        ticketTemplateDtoRequest.setDeleted(null);
        ticketTemplateDtoRequest.setZoneId(ticketTemplate.getZoneId() + "blabla");
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
                        .jsonPath("$.errors.zoneId[?(@.message == '%s')]", ZONE_ID_NOT_VALID).exists());

        verify(accountService, times(0)).findById(any());
        verify(templateService, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldBadRequestWithErrorsDescriptions_whenMustConsistExpressionSchedule() throws Exception {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        TicketTemplateDtoRequest ticketTemplateDtoRequest = ticketTemplateTestHelper.convertEntityToDtoRequest(ticketTemplate);
        ticketTemplateDtoRequest.setId(null);
        ticketTemplateDtoRequest.setVersion(null);
        ticketTemplateDtoRequest.setDeleted(null);
        ticketTemplateDtoRequest.setExpressionSchedule("1 1 1 1 1 1 1");
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
                        .jsonPath("$.errors.expressionSchedule[?(@.message =~ /%s.*/)]", EXPRESSION_MUST_CONSIST).exists());

        verify(accountService, times(0)).findById(any());
        verify(templateService, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldBadRequestWithErrorsDescriptions_whenInvalidValueExpressionSchedule() throws Exception {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        TicketTemplateDtoRequest ticketTemplateDtoRequest = ticketTemplateTestHelper.convertEntityToDtoRequest(ticketTemplate);
        ticketTemplateDtoRequest.setId(null);
        ticketTemplateDtoRequest.setVersion(null);
        ticketTemplateDtoRequest.setDeleted(null);
        ticketTemplateDtoRequest.setExpressionSchedule("60 60 24 32 13 8");
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
                        .jsonPath("$.errors.expressionSchedule[?(@.message =~ /%s.*/)]", INVALID_VALUE_FOR).exists());


        verify(accountService, times(0)).findById(any());
        verify(templateService, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldCreate_whenValidDataPassed() throws Exception {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        TicketTemplateDtoRequest ticketTemplateDtoRequest = ticketTemplateTestHelper.convertEntityToDtoRequest(ticketTemplate);
        when(templateService.update(any())).thenReturn(ticketTemplate);
        when(accountService.findById(any())).thenReturn(ticketTemplate.getAccount());
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketTemplateDtoRequest));

        var requestResult = mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andReturn()
                .getResponse()
                .getContentAsString();


        TicketTemplateDtoResponse actualTicketTemplateDtoResponse =
                objectMapper.readValue(requestResult, TicketTemplateDtoResponse.class);

        TicketTemplateDtoResponse expectedTicketTemplateDtoResponse =
                mapper.map(ticketTemplate, TicketTemplateDtoResponse.class);

        assertEquals(expectedTicketTemplateDtoResponse, actualTicketTemplateDtoResponse);

        verify(accountService, times(1)).findById(any());
        verify(templateService, times(1)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldBadRequestWithErrorsDescriptions_whenInvalidData() throws Exception {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        TicketTemplateDtoRequest ticketTemplateDtoRequest = ticketTemplateTestHelper.convertEntityToDtoRequest(ticketTemplate);
        ticketTemplateDtoRequest.setZoneId(null);
        ticketTemplateDtoRequest.setExpressionSchedule(null);
        ticketTemplateDtoRequest.setAuthorId(null);
        ticketTemplateDtoRequest.setSubject(null);
        ticketTemplateDtoRequest.setIsActive(null);
        ticketTemplateDtoRequest.setIsOnlyOneTicketInWork(null);
        ticketTemplateDtoRequest.setId(null);
        ticketTemplateDtoRequest.setVersion(null);
        ticketTemplateDtoRequest.setDeleted(null);
        when(templateService.update(any())).thenReturn(ticketTemplate);
        when(accountService.findById(any())).thenReturn(ticketTemplate.getAccount());
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
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
                        .jsonPath("$.errors.id[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.version[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.deleted[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.authorId[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.subject[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.isActive[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.isOnlyOneTicketInWork[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists());

        verify(accountService, times(0)).findById(any());
        verify(templateService, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getById_shouldFindOneEntity_whenIdExistInDatabaseByPassedId() throws Exception {
        var ticketTemplate = ticketTemplateTestHelper.getRandomValidEntity();
        when(templateService.findByIdAndAccountId(any(), any())).thenReturn(ticketTemplate);
        mockMvc.perform(get(HOST + PORT + API + ticketTemplate.getId()))
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.subject").value(ticketTemplate.getSubject()))
                .andExpect(jsonPath("$.id").value(ticketTemplate.getId().toString()));
        verify(templateService, times(1)).findByIdAndAccountId(any(), any());
    }

    @Test
    @WithAnonymousUser
    void getById_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        mockMvc.perform(get(HOST + PORT + API + UUID.randomUUID()))
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(templateService, times(0)).findByIdAndAccountId(any(), any());

    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getById_shouldRespondNotFound_whenPassedIdNotExist() throws Exception {
        when(templateService.findByIdAndAccountId(any(), any())).thenThrow(EntityNotExistException.class);
        mockMvc.perform(get(HOST + PORT + API + UUID.randomUUID()))
                .andDo(print())
                .andExpect(status().isNotFound());
        verify(templateService, times(1)).findByIdAndAccountId(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getById_shouldGetStatusBadRequest_whenIdIsInvalid() throws Exception {
        mockMvc.perform(get(HOST + PORT + API + "Abracadabra"))
                .andDo(print())
                .andExpect(status().isBadRequest());
        verify(templateService, times(0)).findById(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getByFilter_shouldGetStatusBadRequest_whenIdIsInvalid() throws Exception {
        var dateFilter = NumberFilter.builder()
                .typeComparison("isbetween_exclusion")
                .valueOne(1.9)
                .valueTwo(1L)
                .build();
        var subF = StringFilter.builder()
                .typeComparison("textcontains")
                .value("dd")
                .build();
        var ticketTypeFilterDto = TicketTemplateFilterDto.builder()
                .dateEnd(dateFilter)
                .subject(subF)
                .build();
        when(templateService.findAllByFilter(any(), any())).thenReturn(new PageImpl<>(List.of(new TicketTemplate(), new TicketTemplate())));
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketTypeFilterDto));
        mockMvc.perform(request)
                .andExpect(status().isBadRequest())
                .andDo(print());
        verify(templateService, times(0)).findAllByFilter(any(), any());
    }
//
//    @Test
//    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
//    void getByFilter_shouldGetStatusBadRequest_whenIdIsInvalid1() throws Exception {
//        ArrayList<UUID> listTT = new ArrayList<>();
//        listTT.add(UUID.randomUUID());
//        //listTT.add(null);
//
//        var ticketTypeFilterDto = TicketTemplateFilterDto.builder()
//                .dateEnd(21321321321L)
//                .comparisonDataStart(">")
//                .dateStart(21321321321L)
//                .ticketTypeId(listTT)
//                .build();
//        when(templateService.findAllByFilter(any(), any())).thenReturn(new PageImpl<>(List.of(new TicketTemplate(), new TicketTemplate())));
//        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
//                .contentType(MediaType.APPLICATION_JSON)
//                .accept(MediaType.APPLICATION_JSON)
//                .content(objectMapper.writeValueAsString(ticketTypeFilterDto));
//        mockMvc.perform(request)
//                .andDo(print())
//                .andExpect(status().isOk());
//                //.andExpect(MockMvcResultMatchers
//                //        .jsonPath("$.errors.ComparisonDataEnd[?(@.message =~ /%s.*/)]", INVALID_COMPARISON).exists());
//        verify(templateService, times(1)).findAllByFilter(any(), any());
//    }
}