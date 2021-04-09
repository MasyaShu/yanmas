package ru.itterminal.yanmas.tickets.controller;

import static org.mockito.Mockito.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static ru.itterminal.yanmas.commons.util.CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY;
import static ru.itterminal.yanmas.commons.util.CommonConstants.MUST_NOT_BE_NULL;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;

import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
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

import com.fasterxml.jackson.databind.ObjectMapper;

import ru.itterminal.yanmas.aau.service.impl.AccountServiceImpl;
import ru.itterminal.yanmas.aau.util.ReflectionHelper;
import ru.itterminal.yanmas.commons.exception.EntityNotExistException;
import ru.itterminal.yanmas.commons.exception.RestExceptionHandler;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.commons.util.CommonConstants;
import ru.itterminal.yanmas.security.config.TestSecurityConfig;
import ru.itterminal.yanmas.tickets.model.GroupTicketTypes;
import ru.itterminal.yanmas.tickets.model.dto.GroupTicketTypesFilterDto;
import ru.itterminal.yanmas.tickets.model.test.GroupTicketTypesTestHelper;
import ru.itterminal.yanmas.tickets.service.impl.GroupTicketTypesServiceImpl;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {GroupTicketTypesControllerV1.class, FilterChainProxy.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class GroupTicketTypesControllerV1Test {

    @MockBean
    private GroupTicketTypesServiceImpl service;

    @SuppressWarnings("unused")
    @MockBean
    private SpecificationsFactory specFactory;

    @SuppressWarnings("unused")
    @MockBean
    private ReflectionHelper reflectionHelper;

    @SuppressWarnings("unused")
    @MockBean
    private AccountServiceImpl accountService;

    @Autowired
    private GroupTicketTypesControllerV1 controller;

    @Autowired
    FilterChainProxy springSecurityFilterChain;

    @Autowired
    UserDetailsService userDetailsService;

    private MockMvc mockMvc;
    private GroupTicketTypes groupTicketTypes;

    @BeforeAll
    void setUpBeforeAll() {
        groupTicketTypes = testHelper.getRandomValidEntity();
        mockMvc = MockMvcBuilders.standaloneSetup(controller)
                .setControllerAdvice(new RestExceptionHandler())
                .apply(SecurityMockMvcConfigurers.springSecurity(springSecurityFilterChain))
                .build();
    }

    private final ObjectMapper objectMapper = new ObjectMapper();
    private static final String HOST = "http://localhost";
    private static final String PORT = ":8081";
    private static final String API = "api/v1/ticket/type/group";

    private final GroupTicketTypesFilterDto groupTicketTypesFilterDto = new GroupTicketTypesFilterDto();
    private final GroupTicketTypesTestHelper testHelper = new GroupTicketTypesTestHelper();

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldCreate_whenValidDataPassed() throws Exception {
        var groupTicketTypesDtoRequest = testHelper.convertEntityToDtoRequest(groupTicketTypes, true);
        when(service.create(any())).thenReturn(groupTicketTypes);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupTicketTypesDtoRequest));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.id").value(groupTicketTypes.getId().toString()))
                .andExpect(jsonPath("$.name").value(groupTicketTypes.getName()));
        verify(service, times(1)).create(any());
    }

    @Test
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusForbidden_whenCurrentUserHasRoleOfAuthor() throws Exception {
        var groupTicketTypesDtoRequest = testHelper.convertEntityToDtoRequest(groupTicketTypes, true);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupTicketTypesDtoRequest));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        var groupTicketTypesDtoRequest = testHelper.convertEntityToDtoRequest(groupTicketTypes, false);
        groupTicketTypesDtoRequest.setName(null);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupTicketTypesDtoRequest));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(
                        MockMvcResultMatchers
                                .jsonPath("$.errors.name[?(@.message == '%s')]", MUST_NOT_BE_NULL)
                                .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.id[?(@.message == '%s')]",
                                           MUST_BE_NULL_FOR_THE_NEW_ENTITY
                                   ).exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath(
                                           "$.errors.version[?(@.message == '%s')]",
                                           MUST_BE_NULL_FOR_THE_NEW_ENTITY
                                   ).exists())
                .andExpect(
                        MockMvcResultMatchers
                                .jsonPath(
                                        "$.errors.deleted[?(@.message == '%s')]",
                                        MUST_BE_NULL_FOR_THE_NEW_ENTITY
                                ).exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenPassedNameIsEmpty() throws Exception {
        var groupTicketTypesDtoRequest = testHelper.convertEntityToDtoRequest(groupTicketTypes, true);
        groupTicketTypesDtoRequest.setName("");
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupTicketTypesDtoRequest));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(
                        MockMvcResultMatchers
                                .jsonPath("$.errors.name[?(@.message =~ /%s.*/)]", CommonConstants.SIZE_MUST_BE_BETWEEN)
                                .exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithAnonymousUser
    void create_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        var groupTicketTypesDtoRequest = testHelper.convertEntityToDtoRequest(groupTicketTypes, true);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupTicketTypesDtoRequest));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isUnauthorized());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldUpdate_whenValidDataPassed() throws Exception {
        var groupTicketTypesDtoRequest = testHelper.convertEntityToDtoRequest(groupTicketTypes, false);
        when(service.update(any())).thenReturn(groupTicketTypes);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupTicketTypesDtoRequest));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.name").value(groupTicketTypes.getName()))
                .andExpect(jsonPath("$.id").value(groupTicketTypes.getId().toString()));
        verify(service, times(1)).update(any());
    }

    @Test
    @WithAnonymousUser
    void update_shouldGetStatusForbidden_whenCurrentUserAnonymousUser() throws Exception {
        var groupTicketTypesDtoRequest = testHelper.convertEntityToDtoRequest(groupTicketTypes, false);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupTicketTypesDtoRequest));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isUnauthorized());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        var groupTicketTypesDtoRequest = testHelper.convertEntityToDtoRequest(groupTicketTypes, true);
        groupTicketTypesDtoRequest.setName(null);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupTicketTypesDtoRequest));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.name[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.id[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.version[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists())
                .andExpect(MockMvcResultMatchers
                                   .jsonPath("$.errors.deleted[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                   .exists());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenVersionIsNegative() throws Exception {
        var groupTicketTypesDtoRequest = testHelper.convertEntityToDtoRequest(groupTicketTypes, false);
        groupTicketTypesDtoRequest.setVersion(-15);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupTicketTypesDtoRequest));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(
                        MockMvcResultMatchers
                                .jsonPath(
                                        "$.errors.version[?(@.message == '%s')]",
                                        CommonConstants.MUST_BE_GREATER_THAN_OR_EQUAL_TO_0
                                )
                                .exists());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidIdPassed() throws Exception {
        var groupTicketTypesDtoRequest = testHelper.convertEntityToDtoRequest(groupTicketTypes, false);
        groupTicketTypesDtoRequest.setId(UUID.randomUUID());
        groupTicketTypesDtoRequest.setVersion(1);
        String json = objectMapper.writeValueAsString(groupTicketTypesDtoRequest);
        json = json.replace(groupTicketTypesDtoRequest.getId().toString(), "abracadabra");
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
    void getById_shouldFindOneGroupTicketTypes_whenOneExistInDatabaseByPassedId() throws Exception {
        when(service.findByIdAndAccountId(any())).thenReturn(groupTicketTypes);
        mockMvc.perform(get(HOST + PORT + API + "/" + groupTicketTypes.getId().toString()))
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.name").value(groupTicketTypes.getName()))
                .andExpect(jsonPath("$.id").value(groupTicketTypes.getId().toString()));
        verify(service, times(1)).findByIdAndAccountId(any());
    }

    @Test
    @WithAnonymousUser
    void getById_shouldGetStatusForbidden_whenCurrentUserAnonymousUser() throws Exception {
        mockMvc.perform(get(HOST + PORT + API + "/" + groupTicketTypes.getId().toString()))
                .andDo(print())
                .andExpect(status().isUnauthorized());
        verify(service, times(0)).findByIdAndAccountId(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getById_shouldRespondNotFound_whenPassedIdNotExist() throws Exception {
        when(service.findByIdAndAccountId(any())).thenThrow(EntityNotExistException.class);
        mockMvc.perform(get(HOST + PORT + API + "/" + groupTicketTypes.getId().toString()))
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
    void getByFilter_shouldGetStatusForbidden_whenCurrentUserAnonymousUser() throws Exception {
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupTicketTypesFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isUnauthorized());
        verify(service, times(0)).findAllByFilter(any(), any());
    }
}
