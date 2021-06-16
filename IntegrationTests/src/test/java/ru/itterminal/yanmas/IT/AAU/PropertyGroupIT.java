package ru.itterminal.yanmas.IT.AAU;

import io.restassured.RestAssured;
import org.apache.tomcat.util.http.fileupload.MultipartStream;
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
import ru.itterminal.yanmas.commons.exception.error.ApiError;
import ru.itterminal.yanmas.commons.model.dto.BaseEntityDto;
import ru.itterminal.yanmas.security.jwt.JwtProvider;
import ru.itterminal.yanmas.tickets.model.dto.TicketDtoResponse;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static io.restassured.RestAssured.given;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static ru.itterminal.yanmas.IT.util.ITHelper.*;

@SuppressWarnings("unused")
@DataJpaTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class, JwtProvider.class, UserRepository.class})
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@TestPropertySource(properties = {"jwt.token.secret=ksedtob", "jwt.token.expired=8640000", "jwt.token.prefix=Bearer"})
class PropertyGroupIT {

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
        itHelper.createInitialPropertyGroups();
    }


    @Test
    void test(){
        assertTrue(true);
    }

//    @ParameterizedTest(name = "{index} User: {0}")
//    @MethodSource("getStreamAllInitialUsersExceptObservers")
//    @Order(10)
//    void SuccessWhenAuthorOfTicketEqualsCurrentUser(String userKey, User currentUser) {
//        UUID idOfTicketWhichAuthorIdEqualsCurrentUserId = null;
//        var initialTickets = itHelper.getTickets().values();
//        for (TicketDtoResponse initialTicket : initialTickets) {
//            if (initialTicket.getAuthor().getId().equals(currentUser.getId())) {
//                idOfTicketWhichAuthorIdEqualsCurrentUserId = initialTicket.getId();
//                break;
//            }
//        }
//        var ticketDtoResponse = given().
//                when().
//                headers(
//                        "Authorization",
//                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
//                )
//                .contentType(APPLICATION_JSON)
//                .pathParam(ID, idOfTicketWhichAuthorIdEqualsCurrentUserId)
//                .get(TICKET_BY_ID)
//                .then()
//                .log().body()
//                .statusCode(HttpStatus.OK.value())
//                .extract().response().as(TicketDtoResponse.class);
//        assertEquals(ticketDtoResponse.getAuthor().getId(), currentUser.getId());
//    }
//
//    @ParameterizedTest(name = "{index} User: {0}")
//    @MethodSource("getStreamInitialUsersWithRolesAdminAndExecutorFromOuterGroup")
//    @Order(20)
//    void AccessDeniedIfCurrentUserWithRoleAdminOrExecutorFromOuterGroupCanNotReadTicketIfTicketIsFromAnotherGroup(String userKey, User currentUser) {
//        var initialTickets = itHelper.getTickets().values();
//        for (TicketDtoResponse initialTicket : initialTickets) {
//            var groupIdOfInitialTicket = initialTicket.getGroup().getId();
//            var groupIdOfCurrentUser = currentUser.getGroup().getId();
//            if (!groupIdOfInitialTicket.equals(groupIdOfCurrentUser)) {
//                var apiError = given().
//                        when().
//                        headers(
//                                "Authorization",
//                                "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
//                        )
//                        .contentType(APPLICATION_JSON)
//                        .pathParam(ID, initialTicket.getId())
//                        .get(TICKET_BY_ID)
//                        .then()
//                        .log().body()
//                        .statusCode(HttpStatus.FORBIDDEN.value())
//                        .extract().response().as(ApiError.class);
//                assertTrue(apiError.getDetail().startsWith("Current user with role"));
//                assertTrue(apiError.getDetail().endsWith("from outer group can not read ticket if ticket is from another group"));
//            }
//        }
//    }
//
//    @Test
//    @Order(25)
//    void LimitAllInitialUsersOnAllTicketTypes() { //NOSONAR
//        itHelper.limitAllInitialUsersOnAllTicketTypes();
//    }
//
//    @ParameterizedTest(name = "{index} User: {0}")
//    @MethodSource("getStreamAllInitialUsersExceptObservers")
//    @Order(30)
//    void AccessDeniedCurrentUserIsAuthorOfTicketWhenLimitAllInitialUsersOnAllTicketTypes(String userKey, User currentUser) {
//        UUID idOfTicketWhichAuthorIdEqualsCurrentUserId = null;
//        var initialTickets = itHelper.getTickets().values();
//        for (TicketDtoResponse initialTicket : initialTickets) {
//            if (initialTicket.getAuthor().getId().equals(currentUser.getId())) {
//                idOfTicketWhichAuthorIdEqualsCurrentUserId = initialTicket.getId();
//                break;
//            }
//        }
//        var apiError = given().
//                when().
//                headers(
//                        "Authorization",
//                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
//                )
//                .contentType(APPLICATION_JSON)
//                .pathParam(ID, idOfTicketWhichAuthorIdEqualsCurrentUserId)
//                .get(TICKET_BY_ID)
//                .then()
//                .log().body()
//                .statusCode(HttpStatus.FORBIDDEN.value())
//                .extract().response().as(ApiError.class);
//        assertEquals("Access denied, because current user has not permit to ticket type", apiError.getDetail());
//    }
//
//    @ParameterizedTest(name = "{index} User: {0}")
//    @MethodSource("getStreamInitialUsersWithRoleObserverFromInnerGroup")
//    @Order(40)
//    void AccessDeniedCurrentUserIsObserverOfTicketWhenLimitAllInitialUsersOnAllTicketTypes(String userKey, User currentUser) {
//        var initialTickets = itHelper.getTickets().values();
//        for (TicketDtoResponse initialTicket : initialTickets) {
//            var listOfObserversIdOfInitialTicket = initialTicket.getObservers().stream()
//                    .map(BaseEntityDto::getId)
//                    .collect(Collectors.toList());
//            if (listOfObserversIdOfInitialTicket.contains(currentUser.getId())) {
//                var apiError = given().
//                        when().
//                        headers(
//                                "Authorization",
//                                "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
//                        )
//                        .contentType(APPLICATION_JSON)
//                        .pathParam(ID, initialTicket.getId())
//                        .get(TICKET_BY_ID)
//                        .then()
//                        .log().body()
//                        .statusCode(HttpStatus.FORBIDDEN.value())
//                        .extract().response().as(ApiError.class);
//                assertEquals("Access denied, because current user has not permit to ticket type", apiError.getDetail());
//            }
//        }
//    }
//
//    @Test
//    @Order(45)
//    void AllowAllInitialUsersOnAllTicketTypes() { //NOSONAR
//        itHelper.allowAllInitialUsersOnAllTicketTypes();
//    }
//
//
//    @ParameterizedTest(name = "{index} User: {0}")
//    @MethodSource("getStreamInitialUsersWithRoleObserverFromInnerGroup")
//    @Order(50)
//    void SuccessWhenCurrentUserIsObserverOfTicket(String userKey, User currentUser) {
//        var initialTickets = itHelper.getTickets().values();
//        for (TicketDtoResponse initialTicket : initialTickets) {
//            var listOfObserversIdOfInitialTicket = initialTicket.getObservers().stream()
//                    .map(BaseEntityDto::getId)
//                    .collect(Collectors.toList());
//            if (listOfObserversIdOfInitialTicket.contains(currentUser.getId())) {
//                var ticketDtoResponse = given().
//                        when().
//                        headers(
//                                "Authorization",
//                                "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
//                        )
//                        .contentType(APPLICATION_JSON)
//                        .pathParam(ID, initialTicket.getId())
//                        .get(TICKET_BY_ID)
//                        .then()
//                        .log().body()
//                        .statusCode(HttpStatus.OK.value())
//                        .extract().response().as(TicketDtoResponse.class);
//                var listOfObserversIdOfTicketDtoResponse = ticketDtoResponse.getObservers().stream()
//                        .map(BaseEntityDto::getId)
//                        .collect(Collectors.toList());
//                assertTrue(listOfObserversIdOfTicketDtoResponse.contains(currentUser.getId()));
//            }
//        }
//    }
//
//    @ParameterizedTest(name = "{index} User: {0}")
//    @MethodSource("getStreamInitialUsersWithRoleObserver")
//    @Order(60)
//    void AccessDeniedWhenCurrentUserWithRoleObserverAndHeIsNotObserverOfTicket(String userKey, User currentUser) {
//        var initialTickets = itHelper.getTickets().values();
//        for (TicketDtoResponse initialTicket : initialTickets) {
//            var listOfObserversIdOfInitialTicket = initialTicket.getObservers().stream()
//                    .map(BaseEntityDto::getId)
//                    .collect(Collectors.toList());
//            if (!listOfObserversIdOfInitialTicket.contains(currentUser.getId())) {
//                var apiError = given().
//                        when().
//                        headers(
//                                "Authorization",
//                                "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
//                        )
//                        .contentType(APPLICATION_JSON)
//                        .pathParam(ID, initialTicket.getId())
//                        .get(TICKET_BY_ID)
//                        .then()
//                        .log().body()
//                        .statusCode(HttpStatus.FORBIDDEN.value())
//                        .extract().response().as(ApiError.class);
//                assertEquals("Current user with role OBSERVER can not read ticket if ticket has not current user in observers", apiError.getDetail());
//            }
//        }
//    }
//
//    @Test
//    @Order(70)
//    void UnauthorizedHttpStatusForAnonymousUser() {
//        given().
//                when()
//                .pathParam(ID, UUID.randomUUID())
//                .get(TICKET_BY_ID)
//                .then()
//                .log().body()
//                .statusCode(HttpStatus.UNAUTHORIZED.value());
//
//    }
//
//    @ParameterizedTest(name = "{index} User: {0}")
//    @MethodSource("getStreamAllInitialUsersWithRoleAuthor")
//    @Order(80)
//    void AccessDeniedWhenCurrentUserWithRoleAuthorCanNotReadTicketIfTicketHasNotCurrentUserAsAuthorAndHasNotInObservers(String userKey, User currentUser) {
//        var initialTickets = itHelper.getTickets().values();
//        for (TicketDtoResponse initialTicket : initialTickets) {
//            var listOfObserversIdOfInitialTicket = initialTicket.getObservers().stream()
//                    .map(BaseEntityDto::getId)
//                    .collect(Collectors.toList());
//            if (!listOfObserversIdOfInitialTicket.contains(currentUser.getId())
//            && !initialTicket.getAuthor().getId().equals(currentUser.getId())) {
//                var apiError = given().
//                        when().
//                        headers(
//                                "Authorization",
//                                "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
//                        )
//                        .contentType(APPLICATION_JSON)
//                        .pathParam(ID, initialTicket.getId())
//                        .get(TICKET_BY_ID)
//                        .then()
//                        .log().body()
//                        .statusCode(HttpStatus.FORBIDDEN.value())
//                        .extract().response().as(ApiError.class);
//                assertEquals("Current user with role AUTHOR can not read ticket if ticket has not current user as a author and has not in observers", apiError.getDetail());
//            }
//        }
//    }
//
//    private static Stream<Arguments> getStreamAllInitialUsersExceptObservers() {
//        var roles = itHelper.getRoleTestHelper().getRolesByNames(
//                List.of(
//                        ACCOUNT_OWNER,
//                        ADMIN,
//                        EXECUTOR,
//                        AUTHOR
//                )
//        );
//        return itHelper.getStreamUsers(roles, null);
//    }
//
//    private static Stream<Arguments> getStreamAllInitialUsersWithRoleAuthor() {
//        var roles = itHelper.getRoleTestHelper().getRolesByNames(
//                List.of(
//                        AUTHOR
//                )
//        );
//        return itHelper.getStreamUsers(roles, null);
//    }
//
//    private static Stream<Arguments> getStreamInitialUsersWithRolesAdminAndExecutorFromOuterGroup() {
//        var roles = itHelper.getRoleTestHelper().getRolesByNames(
//                List.of(
//                        ADMIN,
//                        EXECUTOR
//                )
//        );
//        return itHelper.getStreamUsers(roles, false);
//    }
//
//    private static Stream<Arguments> getStreamInitialUsersWithRoleObserverFromInnerGroup() {
//        var roles = itHelper.getRoleTestHelper().getRolesByNames(
//                List.of(
//                        OBSERVER
//                )
//        );
//        return itHelper.getStreamUsers(roles, true);
//    }
//
//    private static Stream<Arguments> getStreamInitialUsersWithRoleObserver() {
//        var roles = itHelper.getRoleTestHelper().getRolesByNames(
//                List.of(
//                        OBSERVER
//                )
//        );
//        return itHelper.getStreamUsers(roles, null);
//    }
}
