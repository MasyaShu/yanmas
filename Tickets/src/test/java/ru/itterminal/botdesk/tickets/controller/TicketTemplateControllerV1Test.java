package ru.itterminal.botdesk.tickets.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.web.servlet.HttpEncodingAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.*;
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
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.exception.RestExceptionHandler;
import ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter;
import ru.itterminal.botdesk.commons.model.filter.BooleanFilter;
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
import ru.itterminal.botdesk.tickets.service.impl.TicketTypeServiceImpl;

import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import static org.hamcrest.Matchers.hasSize;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static ru.itterminal.botdesk.commons.model.validator.zoneid.ValidateZoneId.ZONE_ID_NOT_VALID;
import static ru.itterminal.botdesk.commons.util.CommonConstants.INVALID_TYPE_COMPARISON_FOR_VALUE_GIVEN;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketTemplateControllerV1.class, FilterChainProxy.class})
@Import({TestSecurityConfig.class, HttpEncodingAutoConfiguration.class})
@WebMvcTest
@ActiveProfiles("Test")
class TicketTemplateControllerV1Test {

    public static final String INVALID_VALUE_FOR = "Invalid value for";
    private static final String SCHEDULER = "ValidateScheduler";
    private static final String ZONE_ID = "ValidateZoneId";
    public static final String EXPRESSION_MUST_CONSIST = "Cron expression must consist";
    private static final String TEXT_CONTAINS = "text_contains";
    private static final String STRING = "19 character string";
    private static final String EXIST_IN = "exist_in";
    private static final String RANDOM_UUID = "f38b1ec1-d0f1-4e00-b594-647134fdc807";
    private static final String IS_BETWEEN_EXCLUSION = "is_between_exclusion";
    private static final String NONAME = "noname";
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

    @MockBean
    private TicketTypeServiceImpl ticketTypeService;

    @MockBean
    private UserServiceImpl userService;

    private final TicketTemplateTestHelper ticketTemplateTestHelper = new TicketTemplateTestHelper();

    private TicketTemplateFilterDto ticketTypeFilterDto;
    private List<String> sortFieldsList;

    private MockMvc mockMvc;

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
    }

    @BeforeEach
    void setUp() {
        StringFilter stringFilter = StringFilter.builder()
                .typeComparison(TEXT_CONTAINS)
                .value(STRING)
                .build();
        BaseEntityFilter baseEntityFilter = BaseEntityFilter.builder()
                .typeComparison(EXIST_IN)
                .listOfIdEntities(List.of(UUID.fromString(RANDOM_UUID)))
                .build();
        NumberFilter numberFilter = NumberFilter.builder()
                .typeComparison(IS_BETWEEN_EXCLUSION)
                .valueOne(Long.MIN_VALUE)
                .valueTwo(Long.MAX_VALUE)
                .build();
        BooleanFilter booleanFilter = BooleanFilter.builder()
                .value(true)
                .build();
        ticketTypeFilterDto = TicketTemplateFilterDto.builder()
                .dateEnd(numberFilter)
                .subject(stringFilter)
                .isOnlyOneTicketInWork(booleanFilter)
                .ticketType(baseEntityFilter)
                .sortByFields(sortFieldsList)
                .build();
        String permittedFieldsForSort = "deleted, displayName, subject, " +
                "description, dateStart, dateEnd, isOnlyOneTicketInWork, " +
                "isActive, zoneId, dateNextRun";
        sortFieldsList = Arrays.asList(permittedFieldsForSort.toLowerCase().split(", "));
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
        TicketTemplateDtoRequest ticketTemplateDtoRequest =
                ticketTemplateTestHelper.convertEntityToDtoRequest(ticketTemplate);
        ticketTemplateDtoRequest.setVersion(null);
        ticketTemplateDtoRequest.setId(null);
        ticketTemplateDtoRequest.setDeleted(null);
        when(templateService.create(any())).thenReturn(ticketTemplate);
        when(accountService.findById(any())).thenReturn(ticketTemplate.getAccount());
        when(userService.findByIdAndAccountId(any())).thenReturn(ticketTemplate.getAuthor());
        when(ticketTypeService.findByIdAndAccountId(any())).thenReturn(ticketTemplate.getTicketType());
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
        TicketTemplateDtoRequest ticketTemplateDtoRequest =
                ticketTemplateTestHelper.convertEntityToDtoRequest(ticketTemplate);
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
                                   .jsonPath(
                                           "$.errors.id[?(@.message == '%s')]",
                                           CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY
                                   ).exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.version[?(@.message == '%s')]",
                                           CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY
                                   ).exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.deleted[?(@.message == '%s')]",
                                           CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY
                                   ).exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.authorId[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.subject[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.isActive[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.isOnlyOneTicketInWork[?(@.message == '%s')]",
                                           CommonConstants.MUST_NOT_BE_NULL
                                   ).exists());

        verify(accountService, times(0)).findById(any());
        verify(templateService, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldBadRequestWithErrorsDescriptions_whenInvalidZoneId() throws Exception {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        TicketTemplateDtoRequest ticketTemplateDtoRequest =
                ticketTemplateTestHelper.convertEntityToDtoRequest(ticketTemplate);
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
        TicketTemplateDtoRequest ticketTemplateDtoRequest =
                ticketTemplateTestHelper.convertEntityToDtoRequest(ticketTemplate);
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
                                   .jsonPath(
                                           "$.errors.expressionSchedule[?(@.message =~ /%s.*/)]",
                                           EXPRESSION_MUST_CONSIST
                                   ).exists());

        verify(accountService, times(0)).findById(any());
        verify(templateService, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldBadRequestWithErrorsDescriptions_whenInvalidValueExpressionSchedule() throws Exception {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        TicketTemplateDtoRequest ticketTemplateDtoRequest =
                ticketTemplateTestHelper.convertEntityToDtoRequest(ticketTemplate);
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
                                   .jsonPath("$.errors.expressionSchedule[?(@.message =~ /%s.*/)]", INVALID_VALUE_FOR)
                                   .exists());

        verify(accountService, times(0)).findById(any());
        verify(templateService, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldCreate_whenValidDataPassed() throws Exception {
        TicketTemplate ticketTemplate = templateTestHelper.getRandomValidEntity();
        TicketTemplateDtoRequest ticketTemplateDtoRequest =
                ticketTemplateTestHelper.convertEntityToDtoRequest(ticketTemplate);
        when(templateService.update(any())).thenReturn(ticketTemplate);
        when(accountService.findById(any())).thenReturn(ticketTemplate.getAccount());
        when(userService.findByIdAndAccountId(any())).thenReturn(ticketTemplate.getAuthor());
        when(ticketTypeService.findByIdAndAccountId(any())).thenReturn(ticketTemplate.getTicketType());
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .characterEncoding("UTF-8")
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
        TicketTemplateDtoRequest ticketTemplateDtoRequest =
                ticketTemplateTestHelper.convertEntityToDtoRequest(ticketTemplate);
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
                                   .jsonPath("$.errors.id[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.version[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.deleted[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.authorId[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.subject[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.isActive[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.isOnlyOneTicketInWork[?(@.message == '%s')]",
                                           CommonConstants.MUST_NOT_BE_NULL
                                   ).exists());

        verify(accountService, times(0)).findById(any());
        verify(templateService, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getById_shouldFindOneEntity_whenIdExistInDatabaseByPassedId() throws Exception {
        var ticketTemplate = ticketTemplateTestHelper.getRandomValidEntity();
        when(templateService.findByIdAndAccountId(any())).thenReturn(ticketTemplate);
        mockMvc.perform(get(HOST + PORT + API + ticketTemplate.getId()))
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.subject").value(ticketTemplate.getSubject()))
                .andExpect(jsonPath("$.id").value(ticketTemplate.getId().toString()));
        verify(templateService, times(1)).findByIdAndAccountId(any());
    }

    @Test
    @WithAnonymousUser
    void getById_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        mockMvc.perform(get(HOST + PORT + API + UUID.randomUUID()))
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(templateService, times(0)).findByIdAndAccountId(any());

    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getById_shouldRespondNotFound_whenPassedIdNotExist() throws Exception {
        when(templateService.findByIdAndAccountId(any())).thenThrow(EntityNotExistException.class);
        mockMvc.perform(get(HOST + PORT + API + UUID.randomUUID()))
                .andDo(print())
                .andExpect(status().isNotFound());
        verify(templateService, times(1)).findByIdAndAccountId(any());
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
    void getByFilter_shouldGetStatusIsOk_whenValidDataFilter() throws Exception {
        var ticketTemplate = ticketTemplateTestHelper.setPredefinedValidEntityList();
        String[] arrayOfFieldsForSort = sortFieldsList.toArray(new String[0]);
        Pageable pageable =
                PageRequest.of(Integer.parseInt(BaseController.PAGE_DEFAULT_VALUE), Integer.parseInt(
                        BaseController.SIZE_DEFAULT_VALUE),
                               Sort.by(
                                       Sort.Direction.fromString("ASC"),
                                       arrayOfFieldsForSort
                               )
                );
        Page<TicketTemplate> expectedPageOfTicketTemplate =
                new PageImpl<>(List.of(ticketTemplate.get(0), ticketTemplate.get(1)), pageable, 2);
        when(templateService.findAllByFilter(any(), any())).thenReturn(expectedPageOfTicketTemplate);
        when(specFactory.makeSpecificationFromEntityFilterDto(any(), any(), any())).thenReturn(null);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketTypeFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].id").value(ticketTemplate.get(0).getId().toString()))
                .andExpect(jsonPath("$.content[1].id").value(ticketTemplate.get(1).getId().toString()))
                .andExpect(jsonPath("$.content", hasSize(2)));
        verify(templateService, times(1)).findAllByFilter(any(), any());
        verify(specFactory, times(1)).makeSpecificationFromEntityFilterDto(any(), any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getByFilter_shouldGetStatusIsBadRequest_whenInvalidDataFilter() throws Exception {
        ticketTypeFilterDto.getIsOnlyOneTicketInWork().setValue(null);
        ticketTypeFilterDto.getDateEnd().setValueOne(1);
        ticketTypeFilterDto.getDateEnd().setValueTwo(0);
        ticketTypeFilterDto.getSubject().setValue(null);
        ticketTypeFilterDto.getTicketType().setListOfIdEntities(List.of());
        ticketTypeFilterDto.setSortByFields(List.of(NONAME));
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketTypeFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.sortByFields[?(@.message =~ /.*%s.*/)]",
                                           CommonConstants.DO_NOT_MATCH_THE_AVAILABLE_SORT_VALUES
                                   )
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.isOnlyOneTicketInWork[?(@.message =~ /.*%s.*/)]",
                                           CommonConstants.MUST_NOT_BE_NULL
                                   )
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.dateEnd[?(@.message =~ /.*%s.*/)]",
                                           "ValueTwo must be greater ValueOne"
                                   )
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.subject[?(@.message =~ /.*%s.*/)]",
                                           CommonConstants.MUST_NOT_BE_NULL
                                   )
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.ticketType[?(@.message =~ /.*%s.*/)]",
                                           CommonConstants.MUST_NOT_BE_NULL
                                   )
                                   .exists());
        verify(templateService, times(0)).findAllByFilter(any(), any());
        verify(specFactory, times(0)).makeSpecificationFromEntityFilterDto(any(), any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getByFilter_shouldGetStatusIsBadRequest_whenInvalidTypeComparisonFilter() throws Exception {
        ticketTypeFilterDto.getDateEnd().setTypeComparison(NONAME);
        ticketTypeFilterDto.getSubject().setTypeComparison(NONAME);
        ticketTypeFilterDto.getTicketType().setTypeComparison(NONAME);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketTypeFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.dateEnd[?(@.message =~ /.*%s.*/)]",
                                           String.format(INVALID_TYPE_COMPARISON_FOR_VALUE_GIVEN, NONAME)
                                   )
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.subject[?(@.message =~ /.*%s.*/)]",
                                           String.format(INVALID_TYPE_COMPARISON_FOR_VALUE_GIVEN, NONAME)
                                   )
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.ticketType[?(@.message =~ /.*%s.*/)]",
                                           String.format(INVALID_TYPE_COMPARISON_FOR_VALUE_GIVEN, NONAME)
                                   )
                                   .exists());
        verify(templateService, times(0)).findAllByFilter(any(), any());
        verify(specFactory, times(0)).makeSpecificationFromEntityFilterDto(any(), any(), any());
    }
}