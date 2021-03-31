package ru.itterminal.botdesk.IT.Tickets;

import static io.restassured.RestAssured.given;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static ru.itterminal.botdesk.IT.util.ITHelper.ACCOUNT_OWNER;
import static ru.itterminal.botdesk.IT.util.ITHelper.ADMIN;
import static ru.itterminal.botdesk.IT.util.ITHelper.APPLICATION_JSON;
import static ru.itterminal.botdesk.IT.util.ITHelper.AUTHOR;
import static ru.itterminal.botdesk.IT.util.ITHelper.EMPTY_BODY;
import static ru.itterminal.botdesk.IT.util.ITHelper.EXECUTOR;
import static ru.itterminal.botdesk.IT.util.ITHelper.OBSERVER;

import java.util.List;
import java.util.stream.Stream;

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
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.aau.repository.UserRepository;
import ru.itterminal.botdesk.security.jwt.JwtProvider;
import ru.itterminal.botdesk.tickets.model.TicketCounter;

@SuppressWarnings("unused")
@DataJpaTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class, JwtProvider.class})
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@TestPropertySource(properties = {"jwt.token.secret=ksedtob", "jwt.token.expired=8640000", "jwt.token.prefix=Bearer"})
class TicketCounterIT {

    public static final String TOTAL_ELEMENTS = "totalElements";
    public static final String CONTENT = "content";
    public static final String TICKET_COUNTER = "ticket-counter";
    public static final String CURRENT_NUMBER = "currentNumber";

    @Autowired
    private UserRepository userRepository;

    private static final ITHelper itHelper = new ITHelper();
    private final UserTestHelper userTestHelper = new UserTestHelper();

    @BeforeAll
    void beforeAll() {
        RestAssured.useRelaxedHTTPSValidation();
        RestAssured.baseURI = "https://localhost:8080";
        RestAssured.basePath = "/api/v1/";
        itHelper.createAndVerifyAccount(userRepository);
        itHelper.createInitialInnerAndOuterGroups(1, 2);
        itHelper.createInitialUsers();
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(10)
    void successGetTicketCounter(String userKey, User user) {
        var ticketCounter = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .get(TICKET_COUNTER)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(TicketCounter.class);
        assertEquals(0L, ticketCounter.getCurrentNumber());
    }

    @Test
    @Order(20)
    void failedGetTicketCounterByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .put(TICKET_COUNTER)
                .then()
                .log().body()
                .statusCode(HttpStatus.UNAUTHORIZED.value());

    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(30)
    void successUpdateTicketCounter(String userKey, User user) {
        var ticketCounterForUpdate = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .get(TICKET_COUNTER)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(TicketCounter.class);
        var newCurrentNumber = ticketCounterForUpdate.getCurrentNumber() + 1;
        ticketCounterForUpdate.setCurrentNumber(newCurrentNumber);
        ticketCounterForUpdate.setDisplayName(null);
        var updatedTicketCounter = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketCounterForUpdate)
                .put(TICKET_COUNTER)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(TicketCounter.class);
        assertEquals(newCurrentNumber, updatedTicketCounter.getCurrentNumber());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(40)
    void failedUpdateTicketCounter_whenNumberLessThanCurrentNumber(String userKey, User user) {
        var ticketCounterForUpdate = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .get(TICKET_COUNTER)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(TicketCounter.class);
        var newCurrentNumber = ticketCounterForUpdate.getCurrentNumber() - 1;
        ticketCounterForUpdate.setCurrentNumber(newCurrentNumber);
        ticketCounterForUpdate.setDisplayName(null);
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketCounterForUpdate)
                .put(TICKET_COUNTER)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(50)
    void failedUpdateTicketCounter_whenUserIsNotInnerGroupAndNotHaveRoleAccountOwnerOrAdmin(String userKey, User user) {
        if (user.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString()) ||
                (user.getRole().getName().equals(Roles.ADMIN.toString()) && user.getGroup().getIsInner())) {
            return;
        }
        var ticketCounterForUpdate = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .get(TICKET_COUNTER)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(TicketCounter.class);
        var newCurrentNumber = ticketCounterForUpdate.getCurrentNumber() + 1;
        ticketCounterForUpdate.setCurrentNumber(newCurrentNumber);
        ticketCounterForUpdate.setDisplayName(null);
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketCounterForUpdate)
                .put(TICKET_COUNTER)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
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
