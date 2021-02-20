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
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.test.TicketTypeTestHelper;

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
class TicketTypeIT {

    @Autowired
    private UserRepository userRepository;

    private static final ITHelper itHelper = new ITHelper();
    private final TicketTypeTestHelper ticketTypeTestHelper = new TicketTypeTestHelper();

    public static final String TICKET_TYPE_1 = "ticketType_1";


    @BeforeAll
    void beforeAll() {
        RestAssured.useRelaxedHTTPSValidation();
        RestAssured.baseURI = "https://localhost:8080";
        RestAssured.basePath = "/api/v1/";
        itHelper.createAndVerifyAccount(userRepository);
        itHelper.createInitialInnerAndOuterGroups(0, 1);
        var allRolesWithoutAccountOwner = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(
                        ITHelper.ADMIN,
                        ITHelper.AUTHOR,
                        ITHelper.EXECUTOR,
                        ITHelper.OBSERVER
                )
        );
        itHelper.createUsersForEachRoleInGroup(
                itHelper.getOuterGroup().get(OUTER_GROUP + 1), allRolesWithoutAccountOwner);
        itHelper.createUsersForEachRoleInGroup(
                itHelper.getInnerGroup().get(INNER_GROUP + 1), allRolesWithoutAccountOwner);
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(10)
    void successGetByFilterByAllUsers(String userKey, User user) {
        var expectedTicketTypeList = itHelper.getTicketTypes().values();
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .param(SIZE, expectedTicketTypeList.size())
                .get(TICKET_TYPE)
                .then()
                .log().body()
                .body(TOTAL_ELEMENTS, equalTo(expectedTicketTypeList.size()))
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        List<TicketType> actualTicketTypeList = from(response).getList(CONTENT, TicketType.class);
        assertThat(expectedTicketTypeList)
                .usingElementComparatorIgnoringFields(ACCOUNT)
                .containsExactlyInAnyOrderElementsOf(actualTicketTypeList);
    }

    @Test
    @Order(20)
    void failedGetByFilterByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .get(TICKET_TYPE)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());

    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(30)
    void successGetByIdByAllUsers(String userKey, User user) {
        var ticketStatusList = itHelper.getTicketTypes().values();
        for (TicketType ticketStatus : ticketStatusList) {
            given().
                    when()
                    .headers(
                            "Authorization",
                            "Bearer " + itHelper.getTokens().get(user.getEmail()))
                    .pathParam(ID, ticketStatus.getId())
                    .get(TICKET_TYPE_BY_ID)
                    .then()
                    .body(ID, equalTo(ticketStatus.getId().toString()))
                    .log().body()
                    .statusCode(HttpStatus.OK.value());
        }
    }

    @Test
    @Order(40)
    void failedGetByIdByAnonymousUser() {
        var ticketStatusList = itHelper.getTicketTypes().values();
        for (TicketType ticketStatus : ticketStatusList) {
            given().
                    when()
                    .pathParam(ID, ticketStatus.getId())
                    .get(TICKET_TYPE_BY_ID)
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
        var ticketType = ticketTypeTestHelper.getRandomValidEntity();
        var ticketStatusDto = ticketTypeTestHelper.convertEntityToDtoRequest(ticketType, true);
        var actualTicketType = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .post(TICKET_TYPE)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(TicketType.class);
        assertThat(List.of(ticketType))
                .usingElementComparatorOnFields("name", "outId", "comment")
                .containsExactlyInAnyOrderElementsOf(List.of(actualTicketType));
        assertFalse(actualTicketType.getDeleted());
        assertFalse(actualTicketType.getIsPredefinedForNewTicket());
        itHelper.getTicketTypes().put(TICKET_TYPE_1, actualTicketType);
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(60)
    void failedCreateByUsersInnerGroupWithRolesAccountOwnerOrAdminWhenNameIsNotUnique(String userKey, User user) {
        var ticketStatus = ticketTypeTestHelper.getRandomValidEntity();
        var ticketStatusDto = ticketTypeTestHelper.convertEntityToDtoRequest(ticketStatus, true);
        ticketStatusDto.setName(itHelper.getTicketTypes().get(IS_PREDEFINED_FOR_NEW_TICKET).getName());
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .post(TICKET_TYPE)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(70)
    void failedCreateByUsersInnerGroupWithRolesAccountOwnerOrAdminWhenPredefinedFieldsAssigned(String userKey, User user) {
        var ticketStatus = ticketTypeTestHelper.getRandomValidEntity();
        var ticketStatusDto = ticketTypeTestHelper.convertEntityToDtoRequest(ticketStatus, true);
        ticketStatusDto.setIsPredefinedForNewTicket(true);
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .post(TICKET_TYPE)
                .then()
                .body(ERRORS, hasKey(IS_PREDEFINED_FOR_NEW_TICKET))
                .log().body()
                .statusCode(HttpStatus.BAD_REQUEST.value())
                .extract().response().asString();
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesExecutorAndAuthorAndObserver")
    @Order(80)
    void failedCreateByUsersInnerGroupWithRolesExecutorOrAuthorOrObserver(String userKey, User user) {
        var ticketStatus = ticketTypeTestHelper.getRandomValidEntity();
        var ticketStatusDto = ticketTypeTestHelper.convertEntityToDtoRequest(ticketStatus, true);
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .post(TICKET_TYPE)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersOuterGroup")
    @Order(90)
    void failedCreateByAllUsersOuterGroup(String userKey, User user) {
        var ticketStatus = ticketTypeTestHelper.getRandomValidEntity();
        var ticketStatusDto = ticketTypeTestHelper.convertEntityToDtoRequest(ticketStatus, true);
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .post(TICKET_TYPE)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(90)
    void failedCreateByAllUsersAnonymous() {
        var ticketStatus = ticketTypeTestHelper.getRandomValidEntity();
        var ticketStatusDto = ticketTypeTestHelper.convertEntityToDtoRequest(ticketStatus, true);
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .post(TICKET_TYPE)
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
        var ticketStatus = itHelper.getTicketTypes().get(IS_PREDEFINED_FOR_NEW_TICKET);
        ticketStatus.setName(ticketStatus.getName() + "_update");
        var ticketStatusDto = ticketTypeTestHelper.convertEntityToDtoRequest(ticketStatus, false);
        var actualTicketType = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .put(TICKET_TYPE)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(TicketType.class);
        assertThat(List.of(ticketStatus))
                .usingElementComparatorOnFields("name", "outId", "comment", "isPredefinedForNewTicket")
                .containsExactlyInAnyOrderElementsOf(List.of(actualTicketType));
        assertFalse(actualTicketType.getDeleted());
        assertTrue(actualTicketType.getIsPredefinedForNewTicket());
        ticketStatus.setVersion(actualTicketType.getVersion());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(110)
    void failedUpdateByUsersInnerGroupWithRolesAccountOwnerOrAdminWhenNameIsNotUnique(String userKey, User user) {
        var ticketStatus = itHelper.getTicketTypes().get(IS_PREDEFINED_FOR_NEW_TICKET);
        var ticketStatusDto = ticketTypeTestHelper.convertEntityToDtoRequest(ticketStatus, false);
        ticketStatusDto.setName(itHelper.getTicketTypes().get(TICKET_TYPE_1).getName());
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .put(TICKET_TYPE)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(120)
    void failedUpdateByUsersInnerGroupWithRolesAccountOwnerOrAdminWhenPredefinedFieldsAssigned(String userKey, User user) {
        var ticketStatus = ticketTypeTestHelper.getRandomValidEntity();
        var ticketStatusDto = ticketTypeTestHelper.convertEntityToDtoRequest(ticketStatus, false);
        ticketStatusDto.setIsPredefinedForNewTicket(true);
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .put(TICKET_TYPE)
                .then()
                .body(ERRORS, hasKey(IS_PREDEFINED_FOR_NEW_TICKET))
                .log().body()
                .statusCode(HttpStatus.BAD_REQUEST.value())
                .extract().response().asString();
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesExecutorAndAuthorAndObserver")
    @Order(130)
    void failedUpdateByUsersInnerGroupWithRolesExecutorOrAuthorOrObserver(String userKey, User user) {
        var ticketStatus = ticketTypeTestHelper.getRandomValidEntity();
        var ticketStatusDto = ticketTypeTestHelper.convertEntityToDtoRequest(ticketStatus, false);
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .put(TICKET_TYPE)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersOuterGroup")
    @Order(140)
    void failedUpdateByAllUsersOuterGroup(String userKey, User user) {
        var ticketStatus = ticketTypeTestHelper.getRandomValidEntity();
        var ticketStatusDto = ticketTypeTestHelper.convertEntityToDtoRequest(ticketStatus, false);
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .put(TICKET_TYPE)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(150)
    void failedUpdateByAllUsersAnonymous() {
        var ticketStatus = ticketTypeTestHelper.getRandomValidEntity();
        var ticketStatusDto = ticketTypeTestHelper.convertEntityToDtoRequest(ticketStatus, false);
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(ticketStatusDto)
                .put(TICKET_TYPE)
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
