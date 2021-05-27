package ru.itterminal.yanmas.IT.Tickets.GroupTicketTypes;

import io.restassured.RestAssured;
import org.assertj.core.api.AssertionsForInterfaceTypes;
import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;
import ru.itterminal.yanmas.IT.util.ITHelper;
import ru.itterminal.yanmas.IT.util.ITTestConfig;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.repository.UserRepository;
import ru.itterminal.yanmas.commons.exception.error.ApiError;
import ru.itterminal.yanmas.security.jwt.JwtProvider;
import ru.itterminal.yanmas.tickets.model.GroupTicketTypes;
import ru.itterminal.yanmas.tickets.model.test.GroupTicketTypesTestHelper;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static io.restassured.RestAssured.given;
import static io.restassured.path.json.JsonPath.from;
import static java.lang.String.format;
import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static ru.itterminal.yanmas.IT.util.ITHelper.*;
import static ru.itterminal.yanmas.aau.service.validator.EntityValidator.NOT_UNIQUE_MESSAGE;
import static ru.itterminal.yanmas.aau.service.validator.EntityValidator.THIS_NAME;
import static ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl.NOT_UNIQUE_CODE;

@SuppressWarnings({"unused", "deprecation", "SimplifyStreamApiCallChains"})
@DataJpaTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class, JwtProvider.class, UserRepository.class})
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class GroupTicketTypesIT {

    public static final String TICKET_SETTING_IS_EMPTY = "Ticket setting is empty";
    public static final String NAME_IS_OCCUPIED = "name is occupied";
    public static final String ACCOUNT_GROUP_AUTHOR = "Account, Group, Author";
    public static final String ADMIN_INNER_GROUP_1 = "adminInnerGroup_1";
    public static final String CREATED_GROUP_TICKET_TYPES = "Created group ticket types";
    @Autowired
    private UserRepository userRepository;

    private static final ITHelper itHelper = new ITHelper();
    private final GroupTicketTypesTestHelper groupTicketTypesTestHelper = new GroupTicketTypesTestHelper();

    @BeforeAll
    void beforeAll() {
        RestAssured.useRelaxedHTTPSValidation();
        RestAssured.baseURI = "https://localhost:8080";
        RestAssured.basePath = "/api/v1/";
        itHelper.createAndVerifyAccount(userRepository);
        itHelper.createInitialInnerAndOuterGroups(1, 2);
        itHelper.createInitialUsers();
        itHelper.createInitialTicketType();
        itHelper.createInitialGroupTicketTypes();
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(10)
    void successGetByFilterByAllUsersWithEmptyFilter(String userKey, User user) {
        @SuppressWarnings("SimplifyStreamApiCallChains")
        var expectedEntity =
                itHelper.getGroupTicketTypes().values().stream().collect(Collectors.toList());
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .param(SIZE, expectedEntity.size())
                .get(GROUP_TICKET_TYPES)
                .then()
                .log().body()
                .body(TOTAL_ELEMENTS, equalTo(expectedEntity.size()))
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        var actualEntity = from(response).getList(CONTENT, GroupTicketTypes.class);
        assertEquals(expectedEntity.size(), actualEntity.size());
        AssertionsForInterfaceTypes.assertThat(actualEntity)
                .usingElementComparatorIgnoringFields(IGNORE_FIELDS_FOR_COMPARE_GROUP_TICKET_TYPES)
                .containsExactlyInAnyOrderElementsOf(expectedEntity);
        assertThat(actualEntity).usingRecursiveComparison().ignoringActualNullFields()
                .isEqualTo(expectedEntity);
    }

    @Test
    @Order(20)
    void failedGetByFilterByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .get(GROUP_TICKET_TYPES)
                .then()
                .log().body()
                .statusCode(HttpStatus.UNAUTHORIZED.value());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(30)
    void successGetByIdByAllUsers(String userKey, User user) {
        var expectedEntity =
                itHelper.getGroupTicketTypes().get(INITIAL_GROUP_TICKET_TYPES);
        var actualEntity = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .pathParam(ID, expectedEntity.getId())
                .get(GROUP_TICKET_TYPES_BY_ID)
                .then()
                .log().body()
                .body(ID, equalTo(expectedEntity.getId().toString()))
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(GroupTicketTypes.class);
        assertThat(actualEntity).usingRecursiveComparison().ignoringActualNullFields()
                .isEqualTo(expectedEntity);
    }

    @Test
    @Order(40)
    void failedGetByIdByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .pathParam(ID, UUID.randomUUID().toString())
                .get(GROUP_TICKET_TYPES_BY_ID)
                .then()
                .log().body()
                .statusCode(HttpStatus.UNAUTHORIZED.value());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(50)
    void successCreateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin(String userKey, User user) {
        var expectedEntity = itHelper.getGroupTicketTypes().get(INITIAL_GROUP_TICKET_TYPES).toBuilder()
                .name(CREATED_GROUP_TICKET_TYPES + userKey)
                .deleted(null)
                .displayName(null)
                .id(null)
                .version(null)
                .build();
        var request = groupTicketTypesTestHelper.convertEntityToDtoRequest(expectedEntity, true);
        var actualEntity = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(request)
                .post(GROUP_TICKET_TYPES)
                .then()
                .log().body()
                .body(NAME, equalTo(CREATED_GROUP_TICKET_TYPES + userKey))
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(GroupTicketTypes.class);
        assertThat(actualEntity).usingRecursiveComparison()
                .ignoringActualNullFields()
                .ignoringFields(IGNORE_FIELDS_OF_BASE_ENTITY_FOR_COMPARE)
                .ignoringFields(NAME)
                .isEqualTo(expectedEntity);
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(60)
    void failedCreateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin_whenNameIsNotUnique(String userKey,
                                                                                             User user) {
        var createdEntity = itHelper.getGroupTicketTypes().get(INITIAL_GROUP_TICKET_TYPES).toBuilder()
                .name(INITIAL_GROUP_TICKET_TYPES)
                .build();
        var request = groupTicketTypesTestHelper.convertEntityToDtoRequest(createdEntity, true);
        var apiError = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(request)
                .post(GROUP_TICKET_TYPES)
                .then()
                .log().body()
                .body(STATUS, equalTo(HttpStatus.CONFLICT.value()))
                .extract().response().as(ApiError.class);
        assertEquals(
                format(NOT_UNIQUE_MESSAGE, THIS_NAME),
                apiError.getErrors().get(NOT_UNIQUE_CODE).get(0).getMessage()
        );
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(70)
    void failedCreateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin_whenListOfTicketTypesIsNull
            (String userKey, User user) {
        var createdEntity = itHelper.getGroupTicketTypes().get(INITIAL_GROUP_TICKET_TYPES).toBuilder()
                .ticketTypes(null)
                .build();
        var request = groupTicketTypesTestHelper.convertEntityToDtoRequest(createdEntity, true);
        given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(request)
                .post(GROUP_TICKET_TYPES)
                .then()
                .log().body()
                .body(STATUS, equalTo(HttpStatus.BAD_REQUEST.value()));
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(80)
    void failedCreateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin_whenListOfTicketTypesIsEmpty
            (String userKey, User user) {
        var createdEntity = itHelper.getGroupTicketTypes().get(INITIAL_GROUP_TICKET_TYPES).toBuilder()
                .ticketTypes(List.of())
                .build();
        var request = groupTicketTypesTestHelper.convertEntityToDtoRequest(createdEntity, true);
        given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(request)
                .post(GROUP_TICKET_TYPES)
                .then()
                .log().body()
                .body(STATUS, equalTo(HttpStatus.BAD_REQUEST.value()));
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(90)
    void failedCreateByAllUsers_whenUserIsNotInnerGroupAndNotHaveRoleAccountOwnerOrAdmin
            (String userKey, User user) {
        if (user.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString()) ||
                (user.getRole().getName().equals(Roles.ADMIN.toString()) && user.getGroup().getIsInner())) {
            return;
        }
        var createdEntity = itHelper.getGroupTicketTypes().get(INITIAL_GROUP_TICKET_TYPES);
        var request = groupTicketTypesTestHelper.convertEntityToDtoRequest(createdEntity, true);
        given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(request)
                .post(GROUP_TICKET_TYPES)
                .then()
                .log().body()
                .body(STATUS, equalTo(HttpStatus.FORBIDDEN.value()));
    }

    @Test
    @Order(100)
    void failedCreateByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .post(GROUP_TICKET_TYPES)
                .then()
                .log().body()
                .statusCode(HttpStatus.UNAUTHORIZED.value());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(110)
    void successUpdateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin(String userKey, User user) {
        var updatedEntity = itHelper.getGroupTicketTypes().get(INITIAL_GROUP_TICKET_TYPES).toBuilder().build();
        var request = groupTicketTypesTestHelper.convertEntityToDtoRequest(updatedEntity, false);
        var entityAfterUpdate = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(request)
                .put(GROUP_TICKET_TYPES)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(GroupTicketTypes.class);
        itHelper.getGroupTicketTypes().get(INITIAL_GROUP_TICKET_TYPES).setVersion(entityAfterUpdate.getVersion());
        assertThat(entityAfterUpdate).usingRecursiveComparison()
                .ignoringActualNullFields()
                .ignoringFields(IGNORE_FIELDS_OF_BASE_ENTITY_FOR_COMPARE)
                .isEqualTo(updatedEntity);
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(120)
    void failedUpdateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin_whenNameIsNotUnique(String userKey,
                                                                                             User user) {
        var updatedEntity = itHelper.getGroupTicketTypes().get(INITIAL_GROUP_TICKET_TYPES).toBuilder()
                .name(CREATED_GROUP_TICKET_TYPES + userKey)
                .build();
        var request = groupTicketTypesTestHelper.convertEntityToDtoRequest(updatedEntity, false);
        var apiError = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(request)
                .put(GROUP_TICKET_TYPES)
                .then()
                .log().body()
                .body(STATUS, equalTo(HttpStatus.CONFLICT.value()))
                .extract().response().as(ApiError.class);
        assertEquals(
                format(NOT_UNIQUE_MESSAGE, THIS_NAME),
                apiError.getErrors().get(NOT_UNIQUE_CODE).get(0).getMessage()
        );
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(130)
    void failedUpdateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin_whenListOfTicketTypesIsNull
            (String userKey, User user) {
        var updatedEntity = itHelper.getGroupTicketTypes().get(INITIAL_GROUP_TICKET_TYPES).toBuilder()
                .ticketTypes(null)
                .build();
        var request = groupTicketTypesTestHelper.convertEntityToDtoRequest(updatedEntity, false);
        given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(request)
                .put(GROUP_TICKET_TYPES)
                .then()
                .log().body()
                .body(STATUS, equalTo(HttpStatus.BAD_REQUEST.value()));
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(140)
    void failedUpdateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin_whenListOfTicketTypesIsEmpty
            (String userKey, User user) {
        var updatedEntity = itHelper.getGroupTicketTypes().get(INITIAL_GROUP_TICKET_TYPES).toBuilder()
                .ticketTypes(List.of())
                .build();
        var request = groupTicketTypesTestHelper.convertEntityToDtoRequest(updatedEntity, false);
        given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(request)
                .put(GROUP_TICKET_TYPES)
                .then()
                .log().body()
                .body(STATUS, equalTo(HttpStatus.BAD_REQUEST.value()));
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(150)
    void failedUpdateByAllUsers_whenUserIsNotInnerGroupAndNotHaveRoleAccountOwnerOrAdmin
            (String userKey, User user) {
        if (user.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString()) ||
                (user.getRole().getName().equals(Roles.ADMIN.toString()) && user.getGroup().getIsInner())) {
            return;
        }
        var updatedEntity = itHelper.getGroupTicketTypes().get(INITIAL_GROUP_TICKET_TYPES);
        var request = groupTicketTypesTestHelper.convertEntityToDtoRequest(updatedEntity, false);
        given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(request)
                .put(GROUP_TICKET_TYPES)
                .then()
                .log().body()
                .body(STATUS, equalTo(HttpStatus.FORBIDDEN.value()));
    }

    @Test
    @Order(160)
    void failedUpdateByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .put(GROUP_TICKET_TYPES)
                .then()
                .log().body()
                .statusCode(HttpStatus.UNAUTHORIZED.value());
    }

    private static Stream<Arguments> getStreamAllUsersFromInnerGroup_2AndFromOuterGroup_2() {
        return itHelper.getStreamAllUsersFromGroups(
                List.of(
                        itHelper.getInnerGroup().get(INNER_GROUP + "2"),
                        itHelper.getOuterGroup().get(OUTER_GROUP + "2")
                )
        );
    }

    private static Stream<Arguments> getStreamAllUsersFromInnerGroup_1AndFromOuterGroup_1() {
        return itHelper.getStreamAllUsersFromGroups(
                List.of(
                        itHelper.getTicketSettings().get(INNER_GROUP + "1").getGroup(),
                        itHelper.getTicketSettings().get(OUTER_GROUP + "1").getGroup()
                )
        );
    }

    private static Stream<Arguments> getStreamAllUsersInnerGroup() {
        return itHelper.getStreamUsers(itHelper.getRoleTestHelper().getPredefinedValidEntityList(), true);
    }

    private static Stream<Arguments> getStreamAllUsersOuterGroup() {
        return itHelper.getStreamUsers(itHelper.getRoleTestHelper().getPredefinedValidEntityList(), false);
    }

    private static Stream<Arguments> getStreamAllUsers() {
        return itHelper.getStreamUsers(itHelper.getRoleTestHelper().getPredefinedValidEntityList(), null);
    }

    private static Stream<Arguments> getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(
                        ACCOUNT_OWNER,
                        ADMIN
                )
        );
        return itHelper.getStreamUsers(roles, true);
    }

    private static Stream<Arguments> getStreamUsersInnerGroupWithRolesExecutorAndAuthorAndObserver() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(
                        EXECUTOR,
                        AUTHOR,
                        OBSERVER
                )
        );
        return itHelper.getStreamUsers(roles, true);
    }

    private static Stream<Arguments> getStreamUsersFromAnyGroupsWithRolesExecutorAndAuthorAndObserver() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(
                        EXECUTOR,
                        AUTHOR,
                        OBSERVER
                )
        );
        return itHelper.getStreamUsers(roles, null);
    }

    private static Stream<Arguments> getStreamUsersFromOuterGroupsWithRolesAdminAndExecutor() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(
                        ADMIN,
                        EXECUTOR
                )
        );
        return itHelper.getStreamUsers(roles, false);
    }

    private static Stream<Arguments> getStreamUsersInnerGroupWithRolesAccountOwnerAndAdminAndExecutor() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(
                        ACCOUNT_OWNER,
                        ADMIN,
                        EXECUTOR
                )
        );
        return itHelper.getStreamUsers(roles, true);
    }

    private static Stream<Arguments> getStreamAllUsersWithoutRoleObserver() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(
                        ACCOUNT_OWNER,
                        ADMIN,
                        EXECUTOR,
                        AUTHOR
                )
        );
        return itHelper.getStreamUsers(roles, null);
    }

    private static Stream<Arguments> getStreamAllUsersFromOuterGroupsWithoutRoleObserver() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(
                        ADMIN,
                        EXECUTOR,
                        AUTHOR
                )
        );
        return itHelper.getStreamUsers(roles, false);
    }

    private static Stream<Arguments> getStreamAllUsersWithRoleObserver() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(List.of(OBSERVER));
        return itHelper.getStreamUsers(roles, null);
    }

    private static Stream<Arguments> getStreamUsersInnerGroupWithRolesAuthorAndObserver() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(
                        AUTHOR,
                        OBSERVER
                )
        );
        return itHelper.getStreamUsers(roles, true);
    }

    private static Stream<Arguments> getStreamUsersFormAnyGroupWithRolesAuthorAndObserver() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(
                        AUTHOR,
                        OBSERVER
                )
        );
        return itHelper.getStreamUsers(roles, null);
    }

    private static Stream<Arguments> getStreamUsersFromOuterGroupsWithRoleAdmin() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(List.of(ADMIN));
        return itHelper.getStreamUsers(roles, false);
    }
}
