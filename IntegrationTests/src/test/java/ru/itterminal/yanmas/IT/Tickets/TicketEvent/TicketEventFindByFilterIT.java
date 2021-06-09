package ru.itterminal.yanmas.IT.Tickets.TicketEvent;

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
import ru.itterminal.yanmas.IT.util.ITHelper;
import ru.itterminal.yanmas.IT.util.ITTestConfig;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.repository.UserRepository;
import ru.itterminal.yanmas.security.jwt.JwtProvider;
import ru.itterminal.yanmas.tickets.model.dto.TicketDtoResponse;
import ru.itterminal.yanmas.tickets.model.dto.TicketEventDtoResponse;

import java.util.List;
import java.util.stream.Stream;

import static io.restassured.RestAssured.given;
import static io.restassured.path.json.JsonPath.from;
import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static ru.itterminal.yanmas.IT.util.ITHelper.*;

@SuppressWarnings("unused")
@DataJpaTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class, JwtProvider.class, UserRepository.class})
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@TestPropertySource(properties = {"jwt.token.secret=ksedtob", "jwt.token.expired=8640000", "jwt.token.prefix=Bearer"})
class TicketEventFindByFilterIT {

    @Autowired
    private UserRepository userRepository;

    private static final ITHelper itHelper = new ITHelper();

    @BeforeAll
    void beforeAll() {
        RestAssured.useRelaxedHTTPSValidation();
        RestAssured.baseURI = "https://localhost:8080";
        RestAssured.basePath = "/api/v1/";
        itHelper.createAndVerifyAccount(userRepository);
        itHelper.createInitialInnerAndOuterGroups(1, 2);
        itHelper.createInitialUsers();
        itHelper.createInitialTicketType();
        itHelper.createInitialTicketSettings();
        itHelper.createInitialGroupTicketTypes();
        itHelper.createInitialTickets();
        itHelper.createInitialTicketEvents();
    }

    @ParameterizedTest(name = "{index} User: {0}    Ticket: {2}")
    @MethodSource("getStreamAllInitialUsersAllTickets")
    @Order(10)
    void successGetEventsFromTicketIfEventForAllOrForCurrentUserAndCurrentUserIsInTicket(String userKey, User currentUser,
                 String ticketKey, TicketDtoResponse ticket) {
        var expectedListEvent = itHelper.getAvailableTicketEventsFromTicketForUser(currentUser, ticket);
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .pathParam(ID, ticket.getId())
                .get(TICKET_BY_ID + EVENT)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        List<TicketEventDtoResponse> actualTicketStatusList = from(response).getList(CONTENT, TicketEventDtoResponse.class);
        assertEquals(expectedListEvent.size(), actualTicketStatusList.size());
            assertThat(expectedListEvent)
                    .usingElementComparatorOnFields("id")
                    .containsExactlyInAnyOrderElementsOf(actualTicketStatusList);
    }

    @ParameterizedTest(name = "{index} User: {0}    Ticket: {2}")
    @MethodSource("getStreamUsersByRoleExecutorOrAdminOrAccountOwnerWithInnerGroupAndAllTickets")
    @Order(20)
    void successGetEventsFromAllTicketIfEventForAllOrForCurrentUserAndCurrentUserByRoleExecutorOrAdminOrAccountOwnerWithInnerGroup(String userKey, User currentUser,
                                                                                         String ticketKey, TicketDtoResponse ticket) {
        var expectedListEvent = itHelper.getAvailableTicketEventsFromTicketForUser(currentUser, ticket);
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .pathParam(ID, ticket.getId())
                .get(TICKET_BY_ID + EVENT)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        List<TicketEventDtoResponse> actualTicketStatusList = from(response).getList(CONTENT, TicketEventDtoResponse.class);
        assertEquals(expectedListEvent.size(), actualTicketStatusList.size());
        assertThat(expectedListEvent)
                .usingElementComparatorOnFields("id")
                .containsExactlyInAnyOrderElementsOf(actualTicketStatusList);
    }

    @ParameterizedTest(name = "{index} User: {0}    Ticket: {2}")
    @MethodSource("getStreamUsersByRoleExecutorOrAdminWithOuterGroupAndTicketsFromOwnGroup")
    @Order(30)
    void successGetEventsFromTicketIfEventForAllOrForCurrentUserAndCurrentUserByRoleExecutorOrAdminOrAccountOwnerWithOuterGroupAndTicketsFromOwnGroup(String userKey, User currentUser,
                                                                            String ticketKey, TicketDtoResponse ticket) {
        var expectedListEvent = itHelper.getAvailableTicketEventsFromTicketForUser(currentUser, ticket);
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .pathParam(ID, ticket.getId())
                .get(TICKET_BY_ID + EVENT)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        List<TicketEventDtoResponse> actualTicketStatusList = from(response).getList(CONTENT, TicketEventDtoResponse.class);
        assertEquals(expectedListEvent.size(), actualTicketStatusList.size());
        assertThat(expectedListEvent)
                .usingElementComparatorOnFields("id")
                .containsExactlyInAnyOrderElementsOf(actualTicketStatusList);
    }

    @ParameterizedTest(name = "{index} User: {0}    Ticket: {2}")
    @MethodSource("getStreamUsersByRoleAuthorOrObserverAndTicketWithoutUsers")
    @Order(40)
    void failedGetEventsFromTicketIfCurrentUserByRoleExecutorOrAuthorOrObserverAndCurrentUserIsNotInTicket(String userKey, User currentUser,
                                                                                                                      String ticketKey, TicketDtoResponse ticket) {
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .pathParam(ID, ticket.getId())
                .get(TICKET_BY_ID + EVENT)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());

    }

    @ParameterizedTest(name = "{index} User: {0}    Ticket: {2}")
    @MethodSource("getStreamUsersByRoleExecutorOrAdminWithOuterGroupAndTicketsFromNotOwnGroup")
    @Order(50)
    void failedGetEventsFromTicketIfCurrentUserByRoleAdminOrExecutorWithOuterGroupAndTicketNotFromCurrentUserGroup(String userKey, User currentUser,
                                                                                                                   String ticketKey, TicketDtoResponse ticket) {
           given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .pathParam(ID, ticket.getId())
                .get(TICKET_BY_ID + EVENT)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());

    }


    private static Stream<Arguments> getStreamAllInitialUsersAllTickets() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(ACCOUNT_OWNER,
                        ADMIN,
                        EXECUTOR,
                        AUTHOR,
                        OBSERVER
                )
        );
        return itHelper.getStreamUsersAndTickets(roles, null, true, null);
    }

    private static Stream<Arguments> getStreamUsersByRoleExecutorOrAdminOrAccountOwnerWithInnerGroupAndAllTickets() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(ACCOUNT_OWNER,
                        ADMIN,
                        EXECUTOR
                )
        );
        return itHelper.getStreamUsersAndTickets(roles, true, null, null);
    }

    private static Stream<Arguments> getStreamUsersByRoleExecutorOrAdminWithOuterGroupAndTicketsFromOwnGroup() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of( ADMIN,
                        EXECUTOR
                )
        );
        return itHelper.getStreamUsersAndTickets(roles, false, null, true);
    }

    private static Stream<Arguments> getStreamUsersByRoleAuthorOrObserverAndTicketWithoutUsers() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(AUTHOR,
                        OBSERVER
                )
        );
        return itHelper.getStreamUsersAndTickets(roles, null, false, null);
    }

    private static Stream<Arguments> getStreamUsersByRoleExecutorOrAdminWithOuterGroupAndTicketsFromNotOwnGroup() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(ADMIN,
                        EXECUTOR
                )
        );
        return itHelper.getStreamUsersAndTickets(roles, false, null, false);
    }

}
