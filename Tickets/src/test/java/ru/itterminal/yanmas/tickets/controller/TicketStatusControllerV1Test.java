package ru.itterminal.yanmas.tickets.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.*;
import org.springframework.http.MediaType;
import org.springframework.security.core.userdetails.UserDetailsService;
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
import ru.itterminal.yanmas.aau.service.impl.AccountServiceImpl;
import ru.itterminal.yanmas.aau.util.ReflectionHelper;
import ru.itterminal.yanmas.commons.controller.BaseController;
import ru.itterminal.yanmas.commons.exception.EntityNotExistException;
import ru.itterminal.yanmas.commons.exception.RestExceptionHandler;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.commons.util.CommonConstants;
import ru.itterminal.yanmas.security.config.TestSecurityConfig;
import ru.itterminal.yanmas.tickets.model.TicketStatus;
import ru.itterminal.yanmas.tickets.model.dto.TicketStatusDto;
import ru.itterminal.yanmas.tickets.model.dto.TicketStatusFilterDto;
import ru.itterminal.yanmas.tickets.service.impl.TicketStatusServiceImpl;

import java.util.List;
import java.util.UUID;

import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;

@SuppressWarnings("unused")
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketStatusControllerV1.class, FilterChainProxy.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class TicketStatusControllerV1Test {

    @MockBean
    private TicketStatusServiceImpl service;

    @MockBean
    private ReflectionHelper reflectionHelper;

    @MockBean
    private SpecificationsFactory specFactory;

    @MockBean
    private AccountServiceImpl accountService;

    @Autowired
    private TicketStatusControllerV1 controller;

    @Autowired
    FilterChainProxy springSecurityFilterChain;

    @Autowired
    UserDetailsService userDetailsService;

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
    private static final String API = "api/v1/ticket-status";

    private TicketStatus ticketStatus_1;
    private TicketStatus ticketStatus_2;
    private TicketStatusDto ticketStatusDto;
    private TicketStatusFilterDto ticketStatusFilterDto;

    private static final String TICKET_STATUS_ID_1 = "7f66b241-f8ec-4912-8f58-a4ceef2dd4c9";
    private static final String TICKET_STATUS_ID_2 = "17b13694-1907-4af9-8f5d-bfa444356e73";
    private static final String TICKET_STATUS_NAME_1 = "started";
    private static final String TICKET_STATUS_NAME_2 = "finished";

    @BeforeEach
    void setUpBeforeEach() {
        ticketStatus_1 = TicketStatus
                .builder()
                .name(TICKET_STATUS_NAME_1)
                .build();
        ticketStatus_1.setId(UUID.fromString(TICKET_STATUS_ID_1));

        ticketStatus_2 = TicketStatus
                .builder()
                .name(TICKET_STATUS_NAME_2)
                .build();
        ticketStatus_2.setId(UUID.fromString(TICKET_STATUS_ID_2));

        ticketStatusDto = TicketStatusDto
                .builder()
                .name(TICKET_STATUS_NAME_1)
                .sortIndex(10)
                .build();

        ticketStatusFilterDto = TicketStatusFilterDto
                .builder()
                .build();
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldCreate_whenValidDataPassed() throws Exception {
        ticketStatusDto.setDeleted(null);
        when(service.create(any())).thenReturn(ticketStatus_1);
        when(reflectionHelper.convertRequestDtoIntoEntityWhereNestedObjectsWithOnlyValidId(any(), any())).thenReturn(ticketStatus_1);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketStatusDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.id").value(TICKET_STATUS_ID_1))
                .andExpect(jsonPath("$.name").value(TICKET_STATUS_NAME_1));
        verify(service, times(1)).create(any());
        verify(reflectionHelper, times(1)).convertRequestDtoIntoEntityWhereNestedObjectsWithOnlyValidId(any(), any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusForbidden_whenRoleUserAuthor() throws Exception {
        ticketStatusDto.setDeleted(null);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketStatusDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusForbidden_whenRoleUserExecutor() throws Exception {
        ticketStatusDto.setDeleted(null);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketStatusDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        ticketStatusDto.setName(null);
        ticketStatusDto.setId(UUID.fromString(TICKET_STATUS_ID_2));
        ticketStatusDto.setVersion(1);
        ticketStatusDto.setDeleted(false);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketStatusDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(
                        MockMvcResultMatchers
                                .jsonPath("$.errors.name[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.id[?(@.message == '%s')]", CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.version[?(@.message == '%s')]", CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY).exists())
                .andExpect(
                        MockMvcResultMatchers
                                .jsonPath("$.errors.deleted[?(@.message == '%s')]", CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY).exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenPassedNameIsEmpty() throws Exception {
        ticketStatusDto.setDeleted(null);
        ticketStatusDto.setName("");
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketStatusDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(
                        MockMvcResultMatchers
                                .jsonPath("$.errors.name[?(@.message =~ /%s.*/)]", CommonConstants.SIZE_MUST_BE_BETWEEN).exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithAnonymousUser
    void create_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketStatusDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isUnauthorized());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldUpdate_whenValidDataPassed() throws Exception {
        ticketStatusDto.setId(UUID.fromString(TICKET_STATUS_ID_1));
        ticketStatusDto.setVersion(1);
        ticketStatusDto.setName(TICKET_STATUS_NAME_1);
        ticketStatusDto.setDeleted(false);
        when(service.update(any())).thenReturn(ticketStatus_1);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketStatusDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.name").value(TICKET_STATUS_NAME_1))
                .andExpect(jsonPath("$.id").value(TICKET_STATUS_ID_1));
        verify(service, times(1)).update(any());
    }

    @Test
    @WithAnonymousUser
    void update_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketStatusDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isUnauthorized());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        ticketStatusDto.setName(null);
        ticketStatusDto.setId(null);
        ticketStatusDto.setVersion(null);
        ticketStatusDto.setDeleted(null);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketStatusDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.name[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.id[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.version[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.deleted[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL).exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenVersionIsNegative() throws Exception {
        ticketStatusDto.setVersion(-15);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketStatusDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(
                        MockMvcResultMatchers
                                .jsonPath("$.errors.version[?(@.message == '%s')]", CommonConstants.MUST_BE_GREATER_THAN_OR_EQUAL_TO_0)
                                .exists());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidIdPassed() throws Exception {
        ticketStatusDto.setId(UUID.fromString(TICKET_STATUS_ID_1));
        ticketStatusDto.setVersion(1);
        String json = objectMapper.writeValueAsString(ticketStatusDto);
        json = json.replace(TICKET_STATUS_ID_1, "abracadabra");
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(json);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.title").value(CommonConstants.MESSAGE_NOT_READABLE));
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getById_shouldFindOneGroup_whenGroupExistInDatabaseByPassedId() throws Exception {
        when(service.findByIdAndAccountId(any())).thenReturn(ticketStatus_1);
        mockMvc.perform(get(HOST + PORT + API + "/" + TICKET_STATUS_ID_1))
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.name").value(TICKET_STATUS_NAME_1))
                .andExpect(jsonPath("$.id").value(TICKET_STATUS_ID_1));
        verify(service, times(1)).findByIdAndAccountId(any());
    }

    @Test
    @WithAnonymousUser
    void getById_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        when(service.findByIdAndAccountId(any())).thenReturn(ticketStatus_1);
        mockMvc.perform(get(HOST + PORT + API + "/" + TICKET_STATUS_ID_1))
                .andDo(print())
                .andExpect(status().isUnauthorized());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getById_shouldRespondNotFound_whenPassedIdNotExist() throws Exception {
        when(service.findByIdAndAccountId(any())).thenThrow(EntityNotExistException.class);
        mockMvc.perform(get(HOST + PORT + API + "/" + TICKET_STATUS_ID_1))
                .andDo(print())
                .andExpect(status().isNotFound());
        verify(service, times(1)).findByIdAndAccountId(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getById_shouldGetStatusBadRequest_whenIdIsInvalid() throws Exception {
        mockMvc.perform(get(HOST + PORT + API + "/" + "Abracadabra"))
                .andDo(print())
                .andExpect(status().isBadRequest());
        verify(service, times(0)).findById(any());
    }

    @Test
    @WithAnonymousUser
    void getByFilter_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        Pageable pageable =
                PageRequest.of(Integer.parseInt(BaseController.PAGE_DEFAULT_VALUE), Integer.parseInt(
                        BaseController.SIZE_DEFAULT_VALUE),
                        Sort.by("name").ascending());
        Page<TicketStatus> ticketStatusPageExpected = new PageImpl<>(List.of(ticketStatus_1, ticketStatus_2), pageable, 2);
        when(service.findAllByFilter(any(), any())).thenReturn(ticketStatusPageExpected);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketStatusFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isUnauthorized());
        verify(service, times(0)).findAllByFilter(any(), any());
    }
}
