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
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.util.AAUConstants;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.exception.RestExceptionHandler;
import ru.itterminal.botdesk.commons.util.CommonConstants;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.dto.TicketTypeDto;
import ru.itterminal.botdesk.tickets.model.dto.TicketTypeFilterDto;
import ru.itterminal.botdesk.tickets.model.spec.TicketTypeSpec;
import ru.itterminal.botdesk.tickets.service.impl.TicketTypeServiceImpl;

import java.util.List;
import java.util.UUID;

import static org.hamcrest.Matchers.hasSize;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {TicketTypeControllerV1.class, TicketTypeSpec.class, FilterChainProxy.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles("Test")
class TicketTypeControllerV1Test {

    @MockBean
    private TicketTypeServiceImpl service;

    @MockBean
    private AccountServiceImpl accountService;

    @Autowired
    private TicketTypeControllerV1 controller;

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
    private static final String API = "api/v1/ticket-type/";

    private TicketType ticketType_1;
    private TicketType ticketType_2;
    private TicketTypeDto ticketTypeDto;
    private TicketTypeFilterDto ticketTypeFilterDto;

    private static final String TICKET_TYPES_ID_1 = "f2cebd99-bc22-40fa-a596-695d0a35b316";
    private static final String TICKET_TYPES_ID_2 = "0bcefd3c-7986-4f5a-8918-7e675d1d3f3a";
    private static final String TICKET_TYPES_NAME_1 = "ticketTypes1";
    private static final String TICKET_TYPES_NAME_2 = "ticketTypes2";
    private static final String INVALID_DELETED = "ERROR";
    private static final String INVALID_SORT_BY = "ERROR";
    private static final String INVALID_DIRECTION = "ERROR";
    private static final String INVALID_NAME = "";

    @BeforeEach
    void setUpBeforeEach() {
        ticketType_1 = TicketType
                .builder()
                .name(TICKET_TYPES_NAME_1)
                .build();
        ticketType_1.setId(UUID.fromString(TICKET_TYPES_ID_1));

        ticketType_2 = TicketType
                .builder()
                .name(TICKET_TYPES_NAME_2)
                .build();
        ticketType_2.setId(UUID.fromString(TICKET_TYPES_ID_2));

        ticketTypeDto = TicketTypeDto
                .builder()
                .name(TICKET_TYPES_NAME_1)
                .build();

        ticketTypeFilterDto = TicketTypeFilterDto
                .builder()
                .build();
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldCreate_whenValidDataPassed() throws Exception {
        ticketTypeDto.setDeleted(null);
        when(service.create(any())).thenReturn(ticketType_1);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketTypeDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.id").value(TICKET_TYPES_ID_1))
                .andExpect(jsonPath("$.name").value(TICKET_TYPES_NAME_1));
        verify(service, times(1)).create(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusForbidden_whenRoleUserAuthor() throws Exception {
        ticketTypeDto.setDeleted(null);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketTypeDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusForbidden_whenRoleUserExecutor() throws Exception {
        ticketTypeDto.setDeleted(null);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketTypeDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        ticketTypeDto.setName(null);
        ticketTypeDto.setId(UUID.fromString(TICKET_TYPES_ID_2));
        ticketTypeDto.setVersion(1);
        ticketTypeDto.setDeleted(false);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketTypeDto));
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
        ticketTypeDto.setDeleted(null);
        ticketTypeDto.setName("");
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketTypeDto));
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
                .content(objectMapper.writeValueAsString(ticketTypeDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldUpdate_whenValidDataPassed() throws Exception {
        ticketTypeDto.setId(UUID.fromString(TICKET_TYPES_ID_1));
        ticketTypeDto.setVersion(1);
        ticketTypeDto.setName(TICKET_TYPES_NAME_1);
        ticketTypeDto.setDeleted(false);
        when(service.update(any())).thenReturn(ticketType_1);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketTypeDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.name").value(TICKET_TYPES_NAME_1))
                .andExpect(jsonPath("$.id").value(TICKET_TYPES_ID_1));
        verify(service, times(1)).update(any());
    }

    @Test
    @WithAnonymousUser
    void update_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketTypeDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        ticketTypeDto.setName(null);
        ticketTypeDto.setId(null);
        ticketTypeDto.setVersion(null);
        ticketTypeDto.setDeleted(null);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketTypeDto));
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
        ticketTypeDto.setVersion(-15);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketTypeDto));
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
        ticketTypeDto.setId(UUID.fromString(TICKET_TYPES_ID_1));
        ticketTypeDto.setVersion(1);
        String json = objectMapper.writeValueAsString(ticketTypeDto);
        json = json.replace(TICKET_TYPES_ID_1, "abracadabra");
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
        when(service.findByIdAndAccountId(any(), any())).thenReturn(ticketType_1);
        mockMvc.perform(get(HOST + PORT + API + TICKET_TYPES_ID_1))
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.name").value(TICKET_TYPES_NAME_1))
                .andExpect(jsonPath("$.id").value(TICKET_TYPES_ID_1));
        verify(service, times(1)).findByIdAndAccountId(any(), any());
    }

    @Test
    @WithAnonymousUser
    void getById_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        when(service.findByIdAndAccountId(any(), any())).thenReturn(ticketType_1);
        mockMvc.perform(get(HOST + PORT + API + TICKET_TYPES_ID_1))
                .andDo(print())
                .andExpect(status().isForbidden());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getById_shouldRespondNotFound_whenPassedIdNotExist() throws Exception {
        when(service.findByIdAndAccountId(any(), any())).thenThrow(EntityNotExistException.class);
        mockMvc.perform(get(HOST + PORT + API + TICKET_TYPES_ID_1))
                .andDo(print())
                .andExpect(status().isNotFound());
        verify(service, times(1)).findByIdAndAccountId(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getById_shouldGetStatusBadRequest_whenIdIsInvalid() throws Exception {
        mockMvc.perform(get(HOST + PORT + API + "Abracadabra"))
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
        Page<TicketType> ticketTypesPageExpected = new PageImpl<>(List.of(ticketType_1, ticketType_2), pageable, 2);
        when(service.findAllByFilter(any(), any())).thenReturn(ticketTypesPageExpected);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketTypeFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).findAllByFilter(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getByFilter_shouldFindTwoGroup_whenUsersExistInDatabaseByPassedFilter() throws Exception {
        Pageable pageable =
                PageRequest.of(Integer.parseInt(BaseController.PAGE_DEFAULT_VALUE), Integer.parseInt(
                        BaseController.SIZE_DEFAULT_VALUE),
                        Sort.by("name").ascending());
        Page<TicketType> ticketTypesPageExpected = new PageImpl<>(List.of(ticketType_1, ticketType_2), pageable, 2);
        when(service.findAllByFilter(any(), any())).thenReturn(ticketTypesPageExpected);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketTypeFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].id").value(TICKET_TYPES_ID_1))
                .andExpect(jsonPath("$.content[1].id").value(TICKET_TYPES_ID_2))
                .andExpect(jsonPath("$.content[0].name").value(TICKET_TYPES_NAME_1))
                .andExpect(jsonPath("$.content[1].name").value(TICKET_TYPES_NAME_2))
                .andExpect(jsonPath("$.content", hasSize(2)));
        verify(service, times(1)).findAllByFilter(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getByFilter_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        ticketTypeFilterDto.setName(INVALID_NAME);
        ticketTypeFilterDto.setDeleted(INVALID_DELETED);
        ticketTypeFilterDto.setDirection(INVALID_DIRECTION);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(ticketTypeFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.name[?(@.message =~ /%s.*/)]", CommonConstants.SIZE_MUST_BE_BETWEEN).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.deleted[?(@.message == '%s')]", CommonConstants.MUST_BE_ANY_OF_ALL_TRUE_FALSE).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.direction[?(@.message == '%s')]", CommonConstants.MUST_BE_ANY_OF_ASC_DESC).exists());
        verify(service, times(0)).findAllByFilter(any(), any());
    }


    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void physicalDelete_shouldThrowUnsupportedOperationException_untilMethodWouldBeImplemented() throws Exception {
        mockMvc.perform(delete(HOST + PORT + API + TICKET_TYPES_ID_1))
                .andDo(print())
                .andExpect(status().isMethodNotAllowed());
    }

    @Test
    @WithAnonymousUser
    void physicalDelete_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        mockMvc.perform(delete(HOST + PORT + API + TICKET_TYPES_ID_1))
                .andDo(print())
                .andExpect(status().isForbidden());
    }

    @Test
    @WithAnonymousUser
    void createCheckAccess_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        mockMvc.perform(post(HOST + PORT + API + BaseController.CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isForbidden());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void createCheckAccess_shouldGetStatusOk_whenUserWithRoleAdmin() throws Exception {
        mockMvc.perform(post(HOST + PORT + API + BaseController.CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isOk());
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_2_IS_INNER_GROUP")
    void createCheckAccess_shouldGetStatusOk_whenUserWithRoleAccountOwner() throws Exception {
        mockMvc.perform(post(HOST + PORT + API + BaseController.CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isOk());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void createCheckAccess_shouldGetStatusForbidden_whenUserWithRoleAuthor() throws Exception {
        mockMvc.perform(post(HOST + PORT + API + BaseController.CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isForbidden());
    }

    @Test
    @WithAnonymousUser
    void updateCheckAccess_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        mockMvc.perform(put(HOST + PORT + API + BaseController.CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isForbidden());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void updateCheckAccess_shouldGetStatusOk_whenUserWithRoleAdmin() throws Exception {
        mockMvc.perform(put(HOST + PORT + API + BaseController.CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isOk());
    }

    @Test
    @WithUserDetails("OWNER_ACCOUNT_2_IS_INNER_GROUP")
    void updateCheckAccess_shouldGetStatusOk_whenUserWithRoleAccountOwner() throws Exception {
        mockMvc.perform(put(HOST + PORT + API + BaseController.CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isOk());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void updateCheckAccess_shouldGetStatusForbidden_whenUserWithRoleAuthor() throws Exception {
        mockMvc.perform(put(HOST + PORT + API + BaseController.CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isForbidden());
    }

    @Test
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void updateCheckAccess_shouldGetStatusIsOk_whenUserWithRoleExecutorNotInnerGroup() throws Exception {
        mockMvc.perform(put(HOST + PORT + API + BaseController.CHECK_ACCESS))
                .andDo(print())
                .andExpect(status().isForbidden());
    }
}
