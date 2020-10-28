package ru.itterminal.botdesk.aau.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.mockito.MockitoAnnotations;
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
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.dto.GroupDto;
import ru.itterminal.botdesk.aau.model.dto.GroupFilterDto;
import ru.itterminal.botdesk.aau.model.spec.GroupSpec;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.commons.config.WebTestConfig;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.config.TestSecurityConfig;

import java.util.List;
import java.util.UUID;

import static org.hamcrest.Matchers.hasSize;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static ru.itterminal.botdesk.aau.util.AAUConstants.MUST_BE_ANY_OF_NAME;
import static ru.itterminal.botdesk.commons.controller.BaseController.PAGE_DEFAULT_VALUE;
import static ru.itterminal.botdesk.commons.controller.BaseController.SIZE_DEFAULT_VALUE;
import static ru.itterminal.botdesk.commons.util.CommonConstants.*;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {GroupControllerV1.class, GroupSpec.class, FilterChainProxy.class, AuthorityChecker.class})
@Import(TestSecurityConfig.class)
@WebMvcTest
@ActiveProfiles("Test")
class GroupControllerV1Test {

    @MockBean
    private GroupServiceImpl service;

    @MockBean
    private AccountServiceImpl accountService;

    @Autowired
    private GroupControllerV1 controller;

    @Autowired
    FilterChainProxy springSecurityFilterChain;

    @Autowired
    UserDetailsService userDetailsService;

    @Autowired
    AuthorityChecker authorityChecker;

    private MockMvc mockMvc;

    @BeforeAll
    void setUpBeforeAll() {
        MockitoAnnotations.initMocks(this);
        mockMvc = MockMvcBuilders.standaloneSetup(controller)
                .setControllerAdvice(WebTestConfig.controllerAdvice())
                .apply(SecurityMockMvcConfigurers.springSecurity(springSecurityFilterChain))
                .build();
    }

    private ObjectMapper objectMapper = new ObjectMapper();
    private static String HOST = "http://localhost";
    private static String PORT = ":8081";
    private static String API = "api/v1/group/";

    private Group group_1;
    private Group group_2;
    private GroupDto groypDtoFromAccount_1;
    private GroupFilterDto groupFilterDto;
    private static String GROUP_1_ID = "d592facb-e6ee-4801-8310-9c7708eb6e6c";
    private static String GROUP_2_ID = "86840939-c488-448b-a473-cd9e1097dd32";
    private static String GROUP_NAME_1 = "group_1";
    private static String GROUP_NAME_2 = "group_2";
    private static String INVALID_DELETED = "ERROR";
    private static String INVALID_SORT_BY = "ERROR";
    private static String INVALID_DIRECTION = "ERROR";
    private static String INVALID_NAME = "";

    @BeforeEach
    void setUpBeforeEach() {
        group_1 = new Group().builder()
                .isInner(true)
                .isDeprecated(false)
                .name(GROUP_NAME_1)
                .build();
        group_1.setId(UUID.fromString(GROUP_1_ID));
        group_2 = new Group().builder()
                .isInner(true)
                .isDeprecated(false)
                .name(GROUP_NAME_2)
                .build();
        group_2.setId(UUID.fromString(GROUP_2_ID));

        groypDtoFromAccount_1 = new GroupDto().builder()
                .isInner(true)
                .name("group_1")
                .build();
        groypDtoFromAccount_1.setDeleted(false);
        groypDtoFromAccount_1.setIsDeprecated(false);
        groupFilterDto = new GroupFilterDto();
        groupFilterDto.setName(GROUP_NAME_1);
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    public void create_shouldCreate_whenValidDataPassed() throws Exception {
        groypDtoFromAccount_1.setDeleted(null);
        groypDtoFromAccount_1.setIsDeprecated(null);
        when(service.create(any())).thenReturn(group_1);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groypDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.id").value(GROUP_1_ID))
                .andExpect(jsonPath("$.name").value(GROUP_NAME_1));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    public void create_shouldGetStatusForbidden_whenGroupUserIsNotInner() throws Exception {
        groypDtoFromAccount_1.setDeleted(null);
        groypDtoFromAccount_1.setIsDeprecated(null);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groypDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    public void create_shouldGetStatusForbidden_whenRoleUserAuthor() throws Exception {
        groypDtoFromAccount_1.setDeleted(null);
        groypDtoFromAccount_1.setIsDeprecated(null);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groypDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    public void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        groypDtoFromAccount_1.setName(null);
        groypDtoFromAccount_1.setIsInner(null);
        groypDtoFromAccount_1.setIsDeprecated(false);
        groypDtoFromAccount_1.setId(UUID.fromString(GROUP_1_ID));
        groypDtoFromAccount_1.setVersion(1);
        groypDtoFromAccount_1.setDeleted(false);
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groypDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(
                        jsonPath("$.errors.isInner[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.name[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.isDeprecated[?(@.message == '%s')]", MUST_BE_NULL_FOR_THE_NEW_ENTITY).exists())
                .andExpect(jsonPath("$.errors.id[?(@.message == '%s')]", MUST_BE_NULL_FOR_THE_NEW_ENTITY).exists())
                .andExpect(jsonPath("$.errors.version[?(@.message == '%s')]", MUST_BE_NULL_FOR_THE_NEW_ENTITY).exists())
                .andExpect(jsonPath("$.errors.deleted[?(@.message == '%s')]", MUST_BE_NULL_FOR_THE_NEW_ENTITY).exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    public void create_shouldGetStatusBadRequestWithErrorsDescriptions_whenNameInvalidDataPassed() throws Exception {
        groypDtoFromAccount_1.setDeleted(null);
        groypDtoFromAccount_1.setIsDeprecated(null);
        groypDtoFromAccount_1.setName("");
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groypDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(
                        jsonPath("$.errors.name[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithAnonymousUser
    public void create_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        MockHttpServletRequestBuilder request = post(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groypDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    public void update_shouldUpdate_whenValidDataPassed() throws Exception {
        groypDtoFromAccount_1.setId(UUID.fromString(GROUP_1_ID));
        groypDtoFromAccount_1.setVersion(1);
        groypDtoFromAccount_1.setIsInner(true);
        groypDtoFromAccount_1.setName(GROUP_NAME_1);
        groypDtoFromAccount_1.setIsDeprecated(false);
        groypDtoFromAccount_1.setDeleted(false);
        when(service.update(any())).thenReturn(group_1);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groypDtoFromAccount_1));
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
    public void update_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groypDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    public void update_shouldGetStatusForbidden_whenNotAllowedRole() throws Exception {
        groypDtoFromAccount_1.setId(UUID.fromString(GROUP_1_ID));
        groypDtoFromAccount_1.setVersion(0);
        groypDtoFromAccount_1.setIsDeprecated(false);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groypDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    public void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        groypDtoFromAccount_1.setName(null);
        groypDtoFromAccount_1.setIsInner(null);
        groypDtoFromAccount_1.setIsDeprecated(null);
        groypDtoFromAccount_1.setId(null);
        groypDtoFromAccount_1.setVersion(null);
        groypDtoFromAccount_1.setDeleted(null);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groypDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(
                        jsonPath("$.errors.isInner[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.name[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.isDeprecated[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.id[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.version[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists())
                .andExpect(jsonPath("$.errors.deleted[?(@.message == '%s')]", MUST_NOT_BE_NULL).exists());
        verify(service, times(0)).create(any());
    }
    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    public void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenVersionIsNegative() throws Exception {
        groypDtoFromAccount_1.setVersion(-15);
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groypDtoFromAccount_1));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(
                        jsonPath("$.errors.version[?(@.message == '%s')]", MUST_BE_GREATER_THAN_OR_EQUAL_TO_0)
                                .exists());
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    public void update_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidIdPassed() throws Exception {
        groypDtoFromAccount_1.setId(UUID.fromString(GROUP_1_ID));
        groypDtoFromAccount_1.setVersion(1);
        String json = objectMapper.writeValueAsString(groypDtoFromAccount_1);
        json = json.replace(GROUP_1_ID, "abracadabra");
        MockHttpServletRequestBuilder request = put(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(json);
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.title").value(MESSAGE_NOT_READABLE));
        verify(service, times(0)).update(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    public void getById_shouldFindOneUser_whenUserExistInDatabaseByPassedId() throws Exception {
        when(service.findByIdAndAccountId(any(), any())).thenReturn(group_1);
        mockMvc.perform(get(HOST + PORT + API + GROUP_1_ID))
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.name").value(GROUP_NAME_1))
                .andExpect(jsonPath("$.id").value(GROUP_1_ID));
    }

    @Test
    @WithAnonymousUser
    public void getById_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        when(service.findByIdAndAccountId(any(), any())).thenReturn(group_1);
        mockMvc.perform(get(HOST + PORT + API + GROUP_1_ID))
                .andDo(print())
                .andExpect(status().isForbidden());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void getById_shouldRespondNotFound_whenPassedIdNotExist() throws Exception {
        when(service.findByIdAndAccountId(any(), any())).thenThrow(EntityNotExistException.class);
        mockMvc.perform(get(HOST + PORT + API + GROUP_1_ID))
                .andDo(print())
                .andExpect(status().isNotFound());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    public void getById_shouldGetStatusBadRequest_whenUiidIsInvalid() throws Exception {
        mockMvc.perform(get(HOST + PORT + API + "Abracadabra"))
                .andDo(print())
                .andExpect(status().isBadRequest());
        verify(service, times(0)).findById(any());
    }

    @Test
    @WithAnonymousUser
    public void getByFilter_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        Pageable pageable =
                PageRequest.of(Integer.parseInt(PAGE_DEFAULT_VALUE), Integer.parseInt(SIZE_DEFAULT_VALUE),
                        Sort.by("name").ascending());
        Page<Group> groupPageExpected = new PageImpl<Group>(List.of(group_1, group_2), pageable, 2);
        when(service.findAllByFilter(any(), any())).thenReturn(groupPageExpected);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isForbidden());
        verify(service, times(0)).findAllByFilter(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    public void getByFilter_shouldFindTwoUsers_whenUsersExistInDatabaseByPassedFilter() throws Exception {
        Pageable pageable =
                PageRequest.of(Integer.parseInt(PAGE_DEFAULT_VALUE), Integer.parseInt(SIZE_DEFAULT_VALUE),
                        Sort.by("name").ascending());
        Page<Group> groupPageExpected = new PageImpl<Group>(List.of(group_1, group_2), pageable, 2);
        when(service.findAllByFilter(any(), any())).thenReturn(groupPageExpected);
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
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    public void getByFilter_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidDataPassed() throws Exception {
        groupFilterDto.setName(INVALID_NAME);
        groupFilterDto.setSortBy(INVALID_SORT_BY);
        groupFilterDto.setDeleted(INVALID_DELETED);
        groupFilterDto.setDirection(INVALID_DIRECTION);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errors.name[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists())
                .andExpect(jsonPath("$.errors.deleted[?(@.message == '%s')]", MUST_BE_ANY_OF_ALL_TRUE_FALSE).exists())
                .andExpect(jsonPath("$.errors.direction[?(@.message == '%s')]", MUST_BE_ANY_OF_ASC_DESC).exists())
                .andExpect(jsonPath("$.errors.sortBy[?(@.message == '%s')]", MUST_BE_ANY_OF_NAME)
                        .exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    public void getByFilter_shouldGetStatusBadRequestWithErrorsDescriptions_whenInvalidSizeAndPagePassed()
            throws Exception {
        MockHttpServletRequestBuilder request = get(HOST + PORT + API + "?page=-1&size=0")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.title").value(REQUEST_NOT_READABLE));
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    public void getByFilter_shouldGetStatusBadRequestWithErrorsDescriptions_whenFilterIsEmpty() throws Exception {
        groupFilterDto.setName("");
        groupFilterDto.setSortBy("");
        groupFilterDto.setDeleted("");
        groupFilterDto.setDirection("");
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errors.name[?(@.message =~ /%s.*/)]", SIZE_MUST_BE_BETWEEN).exists())
                .andExpect(jsonPath("$.errors.deleted[?(@.message == '%s')]", MUST_BE_ANY_OF_ALL_TRUE_FALSE).exists())
                .andExpect(jsonPath("$.errors.direction[?(@.message == '%s')]", MUST_BE_ANY_OF_ASC_DESC).exists())
                .andExpect(jsonPath("$.errors.sortBy[?(@.message == '%s')]", MUST_BE_ANY_OF_NAME)
                        .exists());
        verify(service, times(0)).create(any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    public void getByFilter_shouldFindTwoUsers_whenFilterIsNew() throws Exception {
        GroupFilterDto groupFilterDto = new GroupFilterDto();
        Pageable pageable =
                PageRequest.of(Integer.parseInt(PAGE_DEFAULT_VALUE), Integer.parseInt(SIZE_DEFAULT_VALUE),
                        Sort.by("name").ascending());
        Page<Group> groupPageExpected = new PageImpl<Group>(List.of(group_1, group_2), pageable, 2);
        when(service.findAllByFilter(any(), any())).thenReturn(groupPageExpected);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].id").value(GROUP_1_ID))
                .andExpect(jsonPath("$.content[1].id").value(GROUP_2_ID))
                .andExpect(jsonPath("$.content", hasSize(2)));
        verify(service, times(1)).findAllByFilter(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    public void getByFilter_shouldFindTwoUsers_whenDefaultFieldsInFilterIsNull() throws Exception {
        groupFilterDto.setSortBy(null);
        groupFilterDto.setDeleted(null);
        groupFilterDto.setDirection(null);
        Pageable pageable =
                PageRequest.of(Integer.parseInt(PAGE_DEFAULT_VALUE), Integer.parseInt(SIZE_DEFAULT_VALUE),
                        Sort.by("firstName").ascending());
        Page<Group> groupPageExpected = new PageImpl<Group>(List.of(group_1, group_2), pageable, 2);
        when(service.findAllByFilter(any(), any())).thenReturn(groupPageExpected);
        MockHttpServletRequestBuilder request = get(HOST + PORT + API)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(groupFilterDto));
        mockMvc.perform(request)
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].id").value(GROUP_1_ID))
                .andExpect(jsonPath("$.content[1].id").value(GROUP_2_ID))
                .andExpect(jsonPath("$.content", hasSize(2)));
        verify(service, times(1)).findAllByFilter(any(), any());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void physicalDelete_shouldThrowUnsupportedOperationException_untilMethodWouldBeImplemented() throws Exception {
        mockMvc.perform(delete(HOST + PORT + API + GROUP_1_ID))
                .andDo(print())
                .andExpect(status().isMethodNotAllowed());
    }

    @Test
    @WithAnonymousUser
    void physicalDelete_shouldGetStatusForbidden_whenAnonymousUser() throws Exception {
        mockMvc.perform(delete(HOST + PORT + API + GROUP_1_ID))
                .andDo(print())
                .andExpect(status().isForbidden());
    }
}