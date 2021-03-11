package ru.itterminal.botdesk.IT;

import static io.restassured.RestAssured.given;
import static io.restassured.path.json.JsonPath.from;
import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static ru.itterminal.botdesk.IT.UserIT.CONTENT;
import static ru.itterminal.botdesk.IT.UserIT.TOTAL_ELEMENTS;
import static ru.itterminal.botdesk.IT.util.ITHelper.ACCOUNT_OWNER;
import static ru.itterminal.botdesk.IT.util.ITHelper.ADMIN;
import static ru.itterminal.botdesk.IT.util.ITHelper.APPLICATION_JSON;
import static ru.itterminal.botdesk.IT.util.ITHelper.AUTHOR;
import static ru.itterminal.botdesk.IT.util.ITHelper.AUTHOR_ID;
import static ru.itterminal.botdesk.IT.util.ITHelper.EMPTY_BODY;
import static ru.itterminal.botdesk.IT.util.ITHelper.EXECUTOR;
import static ru.itterminal.botdesk.IT.util.ITHelper.ID;
import static ru.itterminal.botdesk.IT.util.ITHelper.IGNORE_FIELDS_FOR_COMPARE_TICKET_SETTING;
import static ru.itterminal.botdesk.IT.util.ITHelper.IGNORE_FIELDS_OF_BASE_ENTITY_FOR_COMPARE;
import static ru.itterminal.botdesk.IT.util.ITHelper.INNER_GROUP;
import static ru.itterminal.botdesk.IT.util.ITHelper.OBSERVER;
import static ru.itterminal.botdesk.IT.util.ITHelper.OUTER_GROUP;
import static ru.itterminal.botdesk.IT.util.ITHelper.SIZE;
import static ru.itterminal.botdesk.IT.util.ITHelper.STATUS;
import static ru.itterminal.botdesk.IT.util.ITHelper.TICKET_SETTING;
import static ru.itterminal.botdesk.IT.util.ITHelper.TICKET_SETTING_BY_AUTHOR;
import static ru.itterminal.botdesk.IT.util.ITHelper.TICKET_SETTING_BY_ID;
import static ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;
import static ru.itterminal.botdesk.tickets.service.validator.TicketSettingOperationValidator.TICKET_SETTING_MUST_NOT_BE_EMPTY;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.assertj.core.api.AssertionsForInterfaceTypes;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;

import io.restassured.RestAssured;
import ru.itterminal.botdesk.IT.util.ITHelper;
import ru.itterminal.botdesk.IT.util.ITTestConfig;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.repository.UserRepository;
import ru.itterminal.botdesk.commons.exception.error.ApiError;
import ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter;
import ru.itterminal.botdesk.security.jwt.JwtProvider;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.dto.TicketSettingFilterDto;
import ru.itterminal.botdesk.tickets.model.test.TicketSettingTestHelper;

@SuppressWarnings({"unused", "deprecation"})
@DataJpaTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class, JwtProvider.class})
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@TestPropertySource(properties = {"jwt.token.secret=ksedtob", "jwt.token.expired=8640000", "jwt.token.prefix=Bearer"})
class TicketSettingIT {

    public static final String TICKET_SETTING_IS_EMPTY = "Ticket setting is empty";
    public static final String ACCOUNT_GROUP_AUTHOR_IS_OCCUPIED = "Account, Group, Author is occupied";
    public static final String ACCOUNT_GROUP_AUTHOR = "Account, Group, Author";
    @Autowired
    private UserRepository userRepository;

    private static final ITHelper itHelper = new ITHelper();
    private final TicketSettingTestHelper ticketSettingTestHelper = new TicketSettingTestHelper();

    @BeforeAll
    void beforeAll() {
        RestAssured.useRelaxedHTTPSValidation();
        RestAssured.baseURI = "https://localhost:8080";
        RestAssured.basePath = "/api/v1/";
        itHelper.createAndVerifyAccount(userRepository);
        itHelper.createInitialInnerAndOuterGroups(1, 2);
        itHelper.createInitialUsers();
        itHelper.createInitialTicketSettings();
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(10)
    void successGetByFilterByAllUsersWithEmptyFilter(String userKey, User user) {
        @SuppressWarnings("SimplifyStreamApiCallChains")
        var expectedTicketSettingList = itHelper.getTicketSettings().values().stream().collect(Collectors.toList());
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .param(SIZE, expectedTicketSettingList.size())
                .get(TICKET_SETTING)
                .then()
                .log().body()
                .body(TOTAL_ELEMENTS, equalTo(expectedTicketSettingList.size()))
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        var actualTicketSettingList = from(response).getList(CONTENT, TicketSetting.class);
        assertEquals(expectedTicketSettingList.size(), actualTicketSettingList.size());
        AssertionsForInterfaceTypes.assertThat(actualTicketSettingList)
                .usingElementComparatorIgnoringFields(IGNORE_FIELDS_FOR_COMPARE_TICKET_SETTING)
                .containsExactlyInAnyOrderElementsOf(expectedTicketSettingList);
        assertThat(actualTicketSettingList).usingRecursiveComparison().ignoringActualNullFields()
                .isEqualTo(expectedTicketSettingList);
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(20)
    void successGetByFilterByAllUsersWithFilterFromGroupOfTicketSetting(String userKey, User user) {
        for (TicketSetting expectedTicketSetting : itHelper.getTicketSettings().values()) {
            var groupFilter = BaseEntityFilter.builder()
                    .typeComparison(EXIST_IN.toString())
                    .listOfIdEntities(List.of(expectedTicketSetting.getGroup().getId()))
                    .build();
            var filter = TicketSettingFilterDto.builder()
                    .group(groupFilter)
                    .build();
            var response = given().
                    when()
                    .headers(
                            "Authorization",
                            "Bearer " + itHelper.getTokens().get(user.getEmail())
                    )
                    .contentType(APPLICATION_JSON)
                    .body(filter)
                    .param(SIZE, 1)
                    .get(TICKET_SETTING)
                    .then()
                    .log().body()
                    .body(TOTAL_ELEMENTS, equalTo(1))
                    .statusCode(HttpStatus.OK.value())
                    .extract().response().asString();
            var actualTicketSettingList = from(response).getList(CONTENT, TicketSetting.class);
            var actualTicketSetting = actualTicketSettingList.get(0);
            assertThat(actualTicketSetting).usingRecursiveComparison().ignoringActualNullFields()
                    .isEqualTo(expectedTicketSetting);
        }
    }

    @Test
    @Order(30)
    void failedGetByFilterByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .get(TICKET_SETTING)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(40)
    void successGetByIdByAllUsers(String userKey, User user) {
        for (TicketSetting expectedTicketSetting : itHelper.getTicketSettings().values()) {
            var actualTicketSetting = given().
                    when().
                    headers(
                            "Authorization",
                            "Bearer " + itHelper.getTokens().get(user.getEmail())
                    )
                    .contentType(APPLICATION_JSON)
                    .pathParam(ID, expectedTicketSetting.getId())
                    .get(TICKET_SETTING_BY_ID)
                    .then()
                    .log().body()
                    .body(ID, equalTo(expectedTicketSetting.getId().toString()))
                    .statusCode(HttpStatus.OK.value())
                    .extract().response().as(TicketSetting.class);
            assertThat(actualTicketSetting).usingRecursiveComparison().ignoringActualNullFields()
                    .isEqualTo(expectedTicketSetting);
        }
    }

    @Test
    @Order(50)
    void failedGetByIdByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .pathParam(ID, UUID.randomUUID().toString())
                .get(TICKET_SETTING_BY_ID)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersFromInnerGroup_1AndFromOuterGroup_1")
    @Order(60)
    void successGetByAuthorByAllUsersFromInnerGroup_1AndFromOuterGroup_1(String userKey, User user) {
        var expectedTicketSetting = user.getGroup().getIsInner()
                ? itHelper.getTicketSettings().get(INNER_GROUP + "1")
                : itHelper.getTicketSettings().get(OUTER_GROUP + "1");
        var actualTicketSetting = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .pathParam(AUTHOR_ID, user.getId())
                .get(TICKET_SETTING_BY_AUTHOR)
                .then()
                .log().body()
                .body(ID, equalTo(expectedTicketSetting.getId().toString()))
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(TicketSetting.class);
        assertThat(actualTicketSetting).usingRecursiveComparison()
                .ignoringActualNullFields()
                .ignoringFields("ticketStatusForCancel", "ticketStatusForClose", "ticketStatusForNew",
                                "ticketStatusForReopen", "ticketTypeForNew"
                )
                .isEqualTo(expectedTicketSetting);
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersFromInnerGroup_2AndFromOuterGroup_2")
    @Order(70)
    void successGetByAuthorByAllUsersFromInnerGroup_2AndFromOuterGroup_2(String userKey, User user) {
        var actualTicketSetting = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .pathParam(AUTHOR_ID, user.getId())
                .get(TICKET_SETTING_BY_AUTHOR)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(TicketSetting.class);
        assertNull(actualTicketSetting.getId());
        assertTrue(actualTicketSetting.getObservers().isEmpty());
        assertTrue(actualTicketSetting.getExecutors().isEmpty());
        assertNotNull(actualTicketSetting.getTicketTypeForNew());
        assertNotNull(actualTicketSetting.getTicketStatusForNew());
        assertNotNull(actualTicketSetting.getTicketStatusForClose());
        assertNotNull(actualTicketSetting.getTicketStatusForCancel());
        assertNotNull(actualTicketSetting.getTicketStatusForReopen());
    }

    @Test
    @Order(80)
    void failedGetByAuthorByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .pathParam(AUTHOR_ID, UUID.randomUUID().toString())
                .get(TICKET_SETTING_BY_AUTHOR)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(90)
    void successCreateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin(String userKey, User user) {
        var expectedTicketSetting = itHelper.getTicketSettings().get(INNER_GROUP + "1").toBuilder()
                .group(user.getGroup())
                .author(user)
                .deleted(null)
                .displayName(null)
                .id(null)
                .version(null)
                .build();
        var dtoRequest = ticketSettingTestHelper.convertEntityToDtoRequest(expectedTicketSetting);
        var actualTicketSetting = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(dtoRequest)
                .post(TICKET_SETTING)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(TicketSetting.class);
        assertThat(actualTicketSetting).usingRecursiveComparison()
                .ignoringActualNullFields()
                .ignoringFields(IGNORE_FIELDS_OF_BASE_ENTITY_FOR_COMPARE)
                .ignoringFields("ticketStatusForCancel", "ticketStatusForClose", "ticketStatusForNew",
                                "ticketStatusForReopen", "ticketTypeForNew"
                )
                .isEqualTo(expectedTicketSetting);
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(100)
    void failedCreateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin_whenTicketSettingIsEmpty
            (String userKey, User user) {
        var newTicketSetting = itHelper.getTicketSettings().get(INNER_GROUP + "1").toBuilder()
                .group(user.getGroup())
                .author(user)
                .observers(null)
                .executors(null)
                .deleted(null)
                .displayName(null)
                .id(null)
                .version(null)
                .build();
        var dtoRequest = ticketSettingTestHelper.convertEntityToDtoRequest(newTicketSetting);
        var apiError = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(dtoRequest)
                .post(TICKET_SETTING)
                .then()
                .log().body()
                .body(STATUS, equalTo(HttpStatus.CONFLICT.value()))
                .extract().response().as(ApiError.class);
        assertEquals(
                TICKET_SETTING_MUST_NOT_BE_EMPTY,
                apiError.getErrors().get(TICKET_SETTING_IS_EMPTY).get(0).getMessage()
        );
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(110)
    void failedCreateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin_whenTicketSettingIsNotUnique
            (String userKey, User user) {
        var newTicketSetting = itHelper.getTicketSettings().get(INNER_GROUP + "1").toBuilder()
                .deleted(null)
                .displayName(null)
                .id(null)
                .version(null)
                .build();
        var dtoRequest = ticketSettingTestHelper.convertEntityToDtoRequest(newTicketSetting);
        var apiError = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(dtoRequest)
                .post(TICKET_SETTING)
                .then()
                .log().body()
                .body(STATUS, equalTo(HttpStatus.CONFLICT.value()))
                .extract().response().as(ApiError.class);
        assertEquals(
                ACCOUNT_GROUP_AUTHOR_IS_OCCUPIED,
                apiError.getErrors().get(ACCOUNT_GROUP_AUTHOR).get(0).getMessage()
        );
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(120)
    void failedCreateByAllUsers_whenUserIsNotInnerGroupAndNotHaveRoleAccountOwnerOrAdmin
            (String userKey, User user) {
        if (user.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString()) ||
                (user.getRole().getName().equals(Roles.ADMIN.toString()) && user.getGroup().getIsInner())) {
            return;
        }
        var newTicketSetting = itHelper.getTicketSettings().get(INNER_GROUP + "1").toBuilder()
                .deleted(null)
                .displayName(null)
                .id(null)
                .version(null)
                .build();
        var dtoRequest = ticketSettingTestHelper.convertEntityToDtoRequest(newTicketSetting);
        given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(dtoRequest)
                .post(TICKET_SETTING)
                .then()
                .log().body()
                .body(STATUS, equalTo(HttpStatus.FORBIDDEN.value()));
    }

    @Test
    @Order(130)
    void failedCreateByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .post(TICKET_SETTING)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(140)
    void successUpdateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin(String userKey, User user) {
        var expectedTicketSetting = itHelper.getTicketSettings().get(INNER_GROUP + "1").toBuilder()
                .displayName(null)
                .build();
        var dtoRequest = ticketSettingTestHelper.convertEntityToDtoRequest(expectedTicketSetting);
        var actualTicketSetting = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(dtoRequest)
                .put(TICKET_SETTING)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(TicketSetting.class);
        itHelper.getTicketSettings().get(INNER_GROUP + "1").setVersion(actualTicketSetting.getVersion());
        assertThat(actualTicketSetting).usingRecursiveComparison()
                .ignoringActualNullFields()
                .ignoringFields(IGNORE_FIELDS_OF_BASE_ENTITY_FOR_COMPARE)
                .ignoringFields("ticketStatusForCancel", "ticketStatusForClose", "ticketStatusForNew",
                                "ticketStatusForReopen", "ticketTypeForNew"
                )
                .isEqualTo(expectedTicketSetting);
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(150)
    void failedUpdateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin_whenAfterUpdateTicketSettingWillBeEmpty(String userKey, User user) {
        var expectedTicketSetting = itHelper.getTicketSettings().get(INNER_GROUP + "1").toBuilder()
                .observers(null)
                .executors(null)
                .displayName(null)
                .build();
        var dtoRequest = ticketSettingTestHelper.convertEntityToDtoRequest(expectedTicketSetting);
        var apiError = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(dtoRequest)
                .put(TICKET_SETTING)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value())
                .extract().response().as(ApiError.class);
        assertEquals(
                TICKET_SETTING_MUST_NOT_BE_EMPTY,
                apiError.getErrors().get(TICKET_SETTING_IS_EMPTY).get(0).getMessage()
        );
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(160)
    void failedUpdateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin_whenAfterUpdateTicketSettingWillBeNotUnique(String userKey, User user) {
        var expectedTicketSetting = itHelper.getTicketSettings().get(INNER_GROUP + "1").toBuilder()
                .group(itHelper.getTicketSettings().get(OUTER_GROUP + "1").getGroup())
                .displayName(null)
                .build();
        var dtoRequest = ticketSettingTestHelper.convertEntityToDtoRequest(expectedTicketSetting);
        var apiError = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(dtoRequest)
                .put(TICKET_SETTING)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value())
                .extract().response().as(ApiError.class);
        assertEquals(
                ACCOUNT_GROUP_AUTHOR_IS_OCCUPIED,
                apiError.getErrors().get(ACCOUNT_GROUP_AUTHOR).get(0).getMessage()
        );
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(170)
    void failedUpdateByAllUsers_whenUserIsNotInnerGroupAndNotHaveRoleAccountOwnerOrAdmin
            (String userKey, User user) {
        if (user.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString()) ||
                (user.getRole().getName().equals(Roles.ADMIN.toString()) && user.getGroup().getIsInner())) {
            return;
        }
        var expectedTicketSetting = itHelper.getTicketSettings().get(INNER_GROUP + "1").toBuilder()
                .displayName(null)
                .build();
        var dtoRequest = ticketSettingTestHelper.convertEntityToDtoRequest(expectedTicketSetting);
        given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(dtoRequest)
                .put(TICKET_SETTING)
                .then()
                .log().body()
                .body(STATUS, equalTo(HttpStatus.FORBIDDEN.value()));
    }

    @Test
    @Order(180)
    void failedUpdateByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .put(TICKET_SETTING)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
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

    private static Stream<Arguments> getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin() {
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