package ru.itterminal.botdesk.IT;


import io.restassured.RestAssured;
import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import ru.itterminal.botdesk.IT.util.ITHelper;
import ru.itterminal.botdesk.IT.util.ITTestConfig;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.repository.UserRepository;
import ru.itterminal.botdesk.security.jwt.JwtProvider;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.test.TicketStatusTestHelper;

import java.util.List;
import java.util.stream.Stream;

import static io.restassured.RestAssured.given;
import static io.restassured.path.json.JsonPath.from;
import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.collection.IsMapContaining.hasKey;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static ru.itterminal.botdesk.IT.util.ITHelper.*;

@DataJpaTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class, JwtProvider.class})
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@TestPropertySource(properties = {"jwt.token.secret=ksedtob", "jwt.token.expired=8640000", "jwt.token.prefix=Bearer"})
class TicketStatusIT {

    @Autowired
    private UserRepository userRepository;

    private static final ITHelper itHelper = new ITHelper();
    private final TicketStatusTestHelper ticketStatusTestHelper = new TicketStatusTestHelper();


    @BeforeAll
    void beforeAll() {
        RestAssured.useRelaxedHTTPSValidation();
        RestAssured.baseURI = "https://localhost:8080";
        RestAssured.basePath = "/api/v1/";
        itHelper.createAndVerifyAccount(userRepository);
        itHelper.createInitialInnerAndOuterGroups(1, 2);
        itHelper.createInitialUsers();
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(10)
    void successGetByFilterByAllUsers(String userKey, User user) {
        var expectedTicketStatusList = itHelper.getTicketStatuses().values();
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .param(SIZE, expectedTicketStatusList.size())
                .get(TICKET_STATUS)
                .then()
                .log().body()
                .body(TOTAL_ELEMENTS, equalTo(expectedTicketStatusList.size()))
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        List<TicketStatus> actualTicketStatusList = from(response).getList(CONTENT, TicketStatus.class);
        assertThat(expectedTicketStatusList)
                .usingElementComparatorIgnoringFields(ACCOUNT)
                .containsExactlyInAnyOrderElementsOf(actualTicketStatusList);
    }

    @Test
    @Order(20)
    void failedGetByFilterByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .get(TICKET_STATUS)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());

    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(30)
    void successGetByIdByAllUsers(String userKey, User user) {
        var ticketStatusList = itHelper.getTicketStatuses().values();
        for (TicketStatus ticketStatus : ticketStatusList) {
            given().
                    when()
                    .headers(
                            "Authorization",
                            "Bearer " + itHelper.getTokens().get(user.getEmail()))
                    .pathParam(ID, ticketStatus.getId())
                    .get(TICKET_STATUS_BY_ID)
                    .then()
                    .body(ID, equalTo(ticketStatus.getId().toString()))
                    .log().body()
                    .statusCode(HttpStatus.OK.value());
        }
    }

    @Test
    @Order(40)
    void failedGetByIdByAnonymousUser() {
        var ticketStatusList = itHelper.getTicketStatuses().values();
        for (TicketStatus ticketStatus : ticketStatusList) {
            given().
                    when()
                    .pathParam(ID, ticketStatus.getId())
                    .get(TICKET_STATUS_BY_ID)
                    .then()
                    .log().body()
                    .statusCode(HttpStatus.FORBIDDEN.value());
        }

    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(50)
    void successCreateByUsersInnerGroupWithRolesAccountOwnerOrAdmin(String userKey, User user) {
        var ticketStatus = ticketStatusTestHelper.getRandomValidEntity();
        var ticketStatusDto = ticketStatusTestHelper.convertEntityToDtoRequest(ticketStatus, true);
        var actualTicketStatus = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .post(TICKET_STATUS)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(TicketStatus.class);
        assertThat(List.of(ticketStatus))
                .usingElementComparatorOnFields("name", "outId", "sortIndex")
                .containsExactlyInAnyOrderElementsOf(List.of(actualTicketStatus));
        assertFalse(actualTicketStatus.getDeleted());
        assertFalse(actualTicketStatus.getIsCanceledPredefined());
        assertFalse(actualTicketStatus.getIsFinishedPredefined());
        assertFalse(actualTicketStatus.getIsReopenedPredefined());
        assertFalse(actualTicketStatus.getIsStartedPredefined());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(60)
    void failedCreateByUsersInnerGroupWithRolesAccountOwnerOrAdminWhenNameIsNotUnique(String userKey, User user) {
        var ticketStatus = ticketStatusTestHelper.getRandomValidEntity();
        var ticketStatusDto = ticketStatusTestHelper.convertEntityToDtoRequest(ticketStatus, true);
        ticketStatusDto.setName(itHelper.getTicketStatuses().get(IS_CANCELED_PREDEFINED).getName());
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .post(TICKET_STATUS)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(70)
    void failedCreateByUsersInnerGroupWithRolesAccountOwnerOrAdminWhenPredefinedFieldsAssigned(String userKey, User user) {
        var ticketStatus = ticketStatusTestHelper.getRandomValidEntity();
        var ticketStatusDto = ticketStatusTestHelper.convertEntityToDtoRequest(ticketStatus, true);
        ticketStatusDto.setIsFinishedPredefined(true);
        ticketStatusDto.setIsReopenedPredefined(true);
        ticketStatusDto.setIsCanceledPredefined(true);
        ticketStatusDto.setIsStartedPredefined(true);
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .post(TICKET_STATUS)
                .then()
                .body(ERRORS, hasKey(IS_CANCELED_PREDEFINED))
                .body(ERRORS, hasKey(IS_FINISHED_PREDEFINED))
                .body(ERRORS, hasKey(IS_REOPENED_PREDEFINED))
                .body(ERRORS, hasKey(IS_STARTED_PREDEFINED))
                .log().body()
                .statusCode(HttpStatus.BAD_REQUEST.value())
                .extract().response().asString();
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesExecutorAndAuthorAndObserver")
    @Order(80)
    void failedCreateByUsersInnerGroupWithRolesExecutorOrAuthorOrObserver(String userKey, User user) {
        var ticketStatus = ticketStatusTestHelper.getRandomValidEntity();
        var ticketStatusDto = ticketStatusTestHelper.convertEntityToDtoRequest(ticketStatus, true);
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .post(TICKET_STATUS)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersOuterGroup")
    @Order(90)
    void failedCreateByAllUsersOuterGroup(String userKey, User user) {
        var ticketStatus = ticketStatusTestHelper.getRandomValidEntity();
        var ticketStatusDto = ticketStatusTestHelper.convertEntityToDtoRequest(ticketStatus, true);
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .post(TICKET_STATUS)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(90)
    void failedCreateByAllUsersAnonymous() {
        var ticketStatus = ticketStatusTestHelper.getRandomValidEntity();
        var ticketStatusDto = ticketStatusTestHelper.convertEntityToDtoRequest(ticketStatus, true);
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .post(TICKET_STATUS)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }


    ////////////////////////
    @SuppressWarnings({"unused", "deprecation"})
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(100)
    void successUpdateByUsersInnerGroupWithRolesAccountOwnerOrAdmin(String userKey, User user) {
        var ticketStatus = itHelper.getTicketStatuses().get(IS_STARTED_PREDEFINED);
        ticketStatus.setName(ticketStatus.getName() + "_update");
        var ticketStatusDto = ticketStatusTestHelper.convertEntityToDtoRequest(ticketStatus, false);
        var actualTicketStatus = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .put(TICKET_STATUS)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(TicketStatus.class);
        assertThat(List.of(ticketStatus))
                .usingElementComparatorOnFields("name", "outId", "sortIndex")
                .containsExactlyInAnyOrderElementsOf(List.of(actualTicketStatus));
        assertFalse(actualTicketStatus.getDeleted());
        assertFalse(actualTicketStatus.getIsCanceledPredefined());
        assertFalse(actualTicketStatus.getIsFinishedPredefined());
        assertFalse(actualTicketStatus.getIsReopenedPredefined());
        assertTrue(actualTicketStatus.getIsStartedPredefined());
        ticketStatus.setVersion(actualTicketStatus.getVersion());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(110)
    void failedUpdateByUsersInnerGroupWithRolesAccountOwnerOrAdminWhenNameIsNotUnique(String userKey, User user) {
        var ticketStatus = itHelper.getTicketStatuses().get(IS_STARTED_PREDEFINED);
        var ticketStatusDto = ticketStatusTestHelper.convertEntityToDtoRequest(ticketStatus, false);
        ticketStatusDto.setName(itHelper.getTicketStatuses().get(IS_CANCELED_PREDEFINED).getName());
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .put(TICKET_STATUS)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(120)
    void failedUpdateByUsersInnerGroupWithRolesAccountOwnerOrAdminWhenPredefinedFieldsAssigned(String userKey, User user) {
        var ticketStatus = ticketStatusTestHelper.getRandomValidEntity();
        var ticketStatusDto = ticketStatusTestHelper.convertEntityToDtoRequest(ticketStatus, false);
        ticketStatusDto.setIsFinishedPredefined(true);
        ticketStatusDto.setIsReopenedPredefined(true);
        ticketStatusDto.setIsCanceledPredefined(true);
        ticketStatusDto.setIsStartedPredefined(true);
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .put(TICKET_STATUS)
                .then()
                .body(ERRORS, hasKey(IS_CANCELED_PREDEFINED))
                .body(ERRORS, hasKey(IS_FINISHED_PREDEFINED))
                .body(ERRORS, hasKey(IS_REOPENED_PREDEFINED))
                .body(ERRORS, hasKey(IS_STARTED_PREDEFINED))
                .log().body()
                .statusCode(HttpStatus.BAD_REQUEST.value())
                .extract().response().asString();
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesExecutorAndAuthorAndObserver")
    @Order(130)
    void failedUpdateByUsersInnerGroupWithRolesExecutorOrAuthorOrObserver(String userKey, User user) {
        var ticketStatus = ticketStatusTestHelper.getRandomValidEntity();
        var ticketStatusDto = ticketStatusTestHelper.convertEntityToDtoRequest(ticketStatus, false);
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .put(TICKET_STATUS)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersOuterGroup")
    @Order(140)
    void failedUpdateByAllUsersOuterGroup(String userKey, User user) {
        var ticketStatus = ticketStatusTestHelper.getRandomValidEntity();
        var ticketStatusDto = ticketStatusTestHelper.convertEntityToDtoRequest(ticketStatus, false);
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .put(TICKET_STATUS)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(150)
    void failedUpdateByAllUsersAnonymous() {
        var ticketStatus = ticketStatusTestHelper.getRandomValidEntity();
        var ticketStatusDto = ticketStatusTestHelper.convertEntityToDtoRequest(ticketStatus, false);
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .put(TICKET_STATUS)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    private static Stream<Arguments> getStreamAllUsers() {
        return itHelper.getStreamUsers(itHelper.getRoleTestHelper().getPredefinedValidEntityList(), false);
    }

    private static Stream<Arguments> getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(ACCOUNT_OWNER,
                        ADMIN
                )
        );
        return itHelper.getStreamUsers(roles, true);
    }

    private static Stream<Arguments> getStreamUsersInnerGroupWithRolesExecutorAndAuthorAndObserver() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(EXECUTOR,
                        AUTHOR,
                        OBSERVER
                )
        );
        return itHelper.getStreamUsers(roles, true);
    }

    private static Stream<Arguments> getStreamAllUsersOuterGroup() {
        return itHelper.getStreamUsers(itHelper.getRoleTestHelper().getPredefinedValidEntityList(), false);
    }


}
