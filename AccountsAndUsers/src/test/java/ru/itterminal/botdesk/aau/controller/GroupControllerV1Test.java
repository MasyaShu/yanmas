package ru.itterminal.botdesk.aau.controller;

import static org.hamcrest.Matchers.hasSize;
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
import static ru.itterminal.botdesk.commons.model.filter.StringFilter.TypeComparisonForStringFilter.TEXT_EQUALS;

import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
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
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.test.context.support.WithAnonymousUser;
import org.springframework.security.test.context.support.WithMockUser;
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

import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.dto.GroupDto;
import ru.itterminal.botdesk.aau.model.dto.GroupFilterDto;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.aau.service.validator.GroupOperationValidator;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.exception.RestExceptionHandler;
import ru.itterminal.botdesk.commons.model.filter.BooleanFilter;
import ru.itterminal.botdesk.commons.model.filter.StringFilter;
import ru.itterminal.botdesk.commons.model.spec.SpecificationsFactory;
import ru.itterminal.botdesk.commons.util.CommonConstants;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;

@SuppressWarnings("unused")
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {GroupControllerV1.class, FilterChainProxy.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles("Test")
class GroupControllerV1Test {

    @MockBean
    private GroupServiceImpl service;

    @MockBean
    private SpecificationsFactory specFactory;

    @MockBean
    private GroupOperationValidator validator;

    @SuppressWarnings("unused")
    @MockBean
    private AccountServiceImpl accountService;

    @Autowired
    private GroupControllerV1 controller;

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
    private static final String API = "api/v1/group";

    private Group group_1;
    private Group group_2;
    private GroupDto groupDtoFromAccount_1;
    private static final String GROUP_1_ID = "d592facb-e6ee-4801-8310-9c7708eb6e6c";
    private static final String GROUP_2_ID = "86840939-c488-448b-a473-cd9e1097dd32";
    private static final String GROUP_NAME_1 = "group_1";
    private static final String GROUP_NAME_2 = "group_2";
    private static final String INVALID_DELETED = "ERROR";
    private static final String INVALID_SORT_BY = "ERROR";
    private static final String INVALID_DIRECTION = "ERROR";
    private static final String INVALID_NAME = "";

    @BeforeEach
    void setUpBeforeEach() {
        group_1 = Group
                .builder()
                .isInner(true)
                .isDeprecated(false)
                .name(GROUP_NAME_1)
                .build();
        group_1.setId(UUID.fromString(GROUP_1_ID));
        group_2 = Group
                .builder()
                .isInner(true)
                .isDeprecated(false)
                .name(GROUP_NAME_2)
                .build();
        group_2.setId(UUID.fromString(GROUP_2_ID));

        groupDtoFromAccount_1 = GroupDto
                .builder()
                .isInner(true)
                .name("group_1")
                .build();
        groupDtoFromAccount_1.setDeleted(false);
        groupDtoFromAccount_1.setIsDeprecated(false);
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldCreate_whenValidDataPassedAndUserAdminInnerGroup() throws Exception {
        groupDtoFromAccount_1.setDeleted(null);
        groupDtoFromAccount_1.setIsDeprecated(null);
        when(service.create(any())).thenReturn(group_1);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.id").value(GROUP_1_ID))
                .andExpect(jsonPath("$.name").value(GROUP_NAME_1));
        verify(service, times(1)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        groupDtoFromAccount_1.setName(null);
        groupDtoFromAccount_1.setIsInner(null);
        groupDtoFromAccount_1.setIsDeprecated(false);
        groupDtoFromAccount_1.setId(UUID.fromString(GROUP_1_ID));
        groupDtoFromAccount_1.setVersion(1);
        groupDtoFromAccount_1.setDeleted(false);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(
                        MockMvcResultMatchers
                                .jsonPath("$.errors.isInner[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                                .exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.name[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                        .exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath(
                                "$.errors.isDeprecated[?(@.message == '%s')]",
                                CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY
                        )
                        .exists())
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
                .andExpect(
                        MockMvcResultMatchers
                                .jsonPath(
                                        "$.errors.deleted[?(@.message == '%s')]",
                                        CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY
                                ).exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenPassedNameIsEmpty() throws Exception {
        groupDtoFromAccount_1.setDeleted(null);
        groupDtoFromAccount_1.setIsDeprecated(null);
        groupDtoFromAccount_1.setName("");
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupDtoFromAccount_1));
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
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isUnauthorized());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithMockUser(authorities = {"EXECUTOR", "AUTHOR", "OBSERVER"})
    void create_shouldGetStatusForbidden_whenUserExecutorOrAuthorOrObserver() throws Exception {
        groupDtoFromAccount_1.setIsDeprecated(null);
        groupDtoFromAccount_1.setDeleted(null);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithMockUser(authorities = {"AUTHOR", "OBSERVER"})
    void update_shouldGetStatusForbidden_whenUserAuthorOrObserver() throws Exception {
        groupDtoFromAccount_1.setVersion(1);
        groupDtoFromAccount_1.setIsInner(null);
        groupDtoFromAccount_1.setId(UUID.fromString(GROUP_1_ID));
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldUpdate_whenValidDataPassed() throws Exception {
        groupDtoFromAccount_1.setId(UUID.fromString(GROUP_1_ID));
        groupDtoFromAccount_1.setVersion(1);
        groupDtoFromAccount_1.setIsInner(null);
        groupDtoFromAccount_1.setName(GROUP_NAME_1);
        groupDtoFromAccount_1.setIsDeprecated(false);
        groupDtoFromAccount_1.setDeleted(false);
        when(service.update(any())).thenReturn(group_1);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.name").value(GROUP_NAME_1))
                .andExpect(jsonPath("$.id").value(GROUP_1_ID))
                .andExpect(jsonPath("$.isInner").value(true))
                .andExpect(jsonPath("$.isDeprecated").value(false));
        verify(service, times(1)).update(any());
    }

    @Test
    @WithAnonymousUser
    void update_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isUnauthorized());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusForbidden_whenNotAllowedRole() throws Exception {
        groupDtoFromAccount_1.setId(UUID.fromString(GROUP_1_ID));
        groupDtoFromAccount_1.setVersion(0);
        groupDtoFromAccount_1.setIsInner(null);
        groupDtoFromAccount_1.setIsDeprecated(false);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        groupDtoFromAccount_1.setName(null);
        groupDtoFromAccount_1.setIsInner(null);
        groupDtoFromAccount_1.setIsDeprecated(null);
        groupDtoFromAccount_1.setId(null);
        groupDtoFromAccount_1.setVersion(null);
        groupDtoFromAccount_1.setDeleted(null);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.name[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                        .exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath(
                                "$.errors.isDeprecated[?(@.message == '%s')]",
                                CommonConstants.MUST_NOT_BE_NULL
                        ).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.id[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                        .exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.version[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                        .exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath("$.errors.deleted[?(@.message == '%s')]", CommonConstants.MUST_NOT_BE_NULL)
                        .exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenVersionIsNegative() throws Exception {
        groupDtoFromAccount_1.setVersion(-15);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupDtoFromAccount_1));
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
        groupDtoFromAccount_1.setId(UUID.fromString(GROUP_1_ID));
        groupDtoFromAccount_1.setVersion(1);
        String json = objectMapper.writeValueAsString(groupDtoFromAccount_1);
        json = json.replace(GROUP_1_ID, "abracadabra");
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
        when(service.findByIdAndAccountId(any())).thenReturn(group_1);
        mockMvc.perform(get(HOST + PORT + API + "/" + GROUP_1_ID))
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.name").value(GROUP_NAME_1))
                .andExpect(jsonPath("$.id").value(GROUP_1_ID));
        verify(service, times(1)).findByIdAndAccountId(any());
    }

    @Test
    @WithAnonymousUser
    void getById_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        when(service.findByIdAndAccountId(any())).thenReturn(group_1);
        mockMvc.perform(get(HOST + PORT + API + "/" + GROUP_1_ID))
                .andDo(print())
                .andExpect(status().isUnauthorized());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getById_shouldReturnNotFound_whenUserIsInInnerGroupAndFindIdForOtherGroup() throws Exception {
        when(service.findByIdAndAccountId(any())).thenReturn(group_1);
        mockMvc.perform(get(HOST + PORT + API + "/" + GROUP_1_ID))
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.name").value(GROUP_NAME_1))
                .andExpect(jsonPath("$.id").value(GROUP_1_ID))
                .andExpect(jsonPath("$.isInner").value(true))
                .andExpect(jsonPath("$.isDeprecated").value(false));
        verify(service, times(1)).findByIdAndAccountId(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getById_shouldRespondNotFound_whenPassedIdNotExist() throws Exception {
        when(service.findByIdAndAccountId(any())).thenThrow(EntityNotExistException.class);
        mockMvc.perform(get(HOST + PORT + API + "/" + GROUP_1_ID))
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
                        Sort.by("name").ascending()
                );
        Page<Group> groupPageExpected = new PageImpl<>(List.of(group_1, group_2), pageable, 2);
        when(service.findAllByFilter(any(), any())).thenReturn(groupPageExpected);
        var groupFilterDto = GroupFilterDto.builder().build();
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isUnauthorized());
        verify(service, times(0)).findAllByFilter(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getByFilter_shouldFindTwoGroup_whenUsersExistInDatabaseByPassedFilter() throws Exception {
        Pageable pageable =
                PageRequest.of(Integer.parseInt(BaseController.PAGE_DEFAULT_VALUE), Integer.parseInt(
                        BaseController.SIZE_DEFAULT_VALUE),
                        Sort.by("name").ascending()
                );
        Page<Group> groupPageExpected = new PageImpl<>(List.of(group_1, group_2), pageable, 2);
        when(service.findAllByFilter(any(), any())).thenReturn(groupPageExpected);
        var groupFilterDto = GroupFilterDto.builder().build();
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].id").value(GROUP_1_ID))
                .andExpect(jsonPath("$.content[1].id").value(GROUP_2_ID))
                .andExpect(jsonPath("$.content[0].name").value(GROUP_NAME_1))
                .andExpect(jsonPath("$.content[1].name").value(GROUP_NAME_2))
                .andExpect(jsonPath("$.content", hasSize(2)));
        verify(service, times(1)).findAllByFilter(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getByFilter_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        var nameFilter = StringFilter.builder()
                .typeComparison(TEXT_EQUALS.toString())
                .value(INVALID_NAME)
                .build();
        var deletedFilter = BooleanFilter.builder()
                .value(null)
                .build();
        var groupFilterDto = GroupFilterDto.builder()
                .name(nameFilter)
                .deleted(deletedFilter)
                .sortDirection(INVALID_DIRECTION)
                .build();
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(MockMvcResultMatchers
                        .jsonPath(
                                "$.errors.name[?(@.message =~ /%s.*/)]",
                                "Value must not be null or empty for comparison"
                        ).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath(
                                "$.errors.deleted[?(@.message == '%s')]",
                                CommonConstants.VALUE_MUST_NOT_BE_NULL
                        ).exists())
                .andExpect(MockMvcResultMatchers
                        .jsonPath(
                                "$.errors.sortDirection[?(@.message == '%s')]",
                                CommonConstants.MUST_BE_ANY_OF_ASC_DESC
                        ).exists());
        verify(service, times(0)).findAllByFilter(any(), any());
    }
}