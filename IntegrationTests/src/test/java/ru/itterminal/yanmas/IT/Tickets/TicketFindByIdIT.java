package ru.itterminal.yanmas.IT.Tickets;

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
import ru.itterminal.yanmas.commons.exception.error.ApiError;
import ru.itterminal.yanmas.security.jwt.JwtProvider;
import ru.itterminal.yanmas.tickets.model.dto.TicketDtoResponse;
import ru.itterminal.yanmas.tickets.model.test.TicketSettingTestHelper;

import java.util.List;
import java.util.UUID;
import java.util.stream.Stream;

import static io.restassured.RestAssured.given;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static ru.itterminal.yanmas.IT.util.ITHelper.*;

@SuppressWarnings({"unused"})
@DataJpaTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class, JwtProvider.class, UserRepository.class})
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@TestPropertySource(properties = {"jwt.token.secret=ksedtob", "jwt.token.expired=8640000", "jwt.token.prefix=Bearer"})
class TicketFindByIdIT {

    public static final String ACCOUNT_GROUP_AUTHOR_IS_OCCUPIED = "Account, Group, Author is occupied";
    public static final String ACCOUNT_GROUP_AUTHOR = "Account, Group, Author";
    public static final String ADMIN_INNER_GROUP_1 = "adminInnerGroup_1";

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
        itHelper.createInitialTicketType();
        itHelper.createInitialTicketSettings();
        itHelper.createInitialGroupTicketTypes();
        itHelper.createInitialTickets();
        itHelper.createTicketTypeWhichIsNeverUsedIntoInitialTickets();
        itHelper.createGroupOfTicketTypesWhichIsNeverUsedIntoInitialTickets();
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllInitialUsersExceptObservers")
    @Order(10)
    void SuccessWhenAuthorOfTicketEqualsCurrentUser(String userKey, User currentUser) {
        UUID idOfTicketWhichAuthorIdEqualsCurrentUserId = null;
        var initialTickets = itHelper.getTickets().values();
        for (TicketDtoResponse initialTicket : initialTickets) {
            if (initialTicket.getAuthor().getId().equals(currentUser.getId())) {
                idOfTicketWhichAuthorIdEqualsCurrentUserId = initialTicket.getId();
                break;
            }
        }
        var ticketDtoResponse = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .pathParam(ID, idOfTicketWhichAuthorIdEqualsCurrentUserId)
                .get(TICKET_BY_ID)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(TicketDtoResponse.class);
        assertEquals(ticketDtoResponse.getAuthor().getId(), currentUser.getId());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamInitialUsersWithRolesAdminAndExecutorFromOuterGroup")
    @Order(20)
    void AccessDeniedIfCurrentUserWithRoleAdminOrExecutorFromOuterGroupCanNotReadTicketIfTicketIsFromAnotherGroup(String userKey, User currentUser) {
        var initialTickets = itHelper.getTickets().values();
        for (TicketDtoResponse initialTicket : initialTickets) {
            var groupIdOfInitialTicket = initialTicket.getGroup().getId();
            var groupIdOfCurrentUser = currentUser.getGroup().getId();
            if (!groupIdOfInitialTicket.equals(groupIdOfCurrentUser)) {
                var apiError = given().
                        when().
                        headers(
                                "Au Fthorization",
                                "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                        )
                        .contentType(APPLICATION_JSON)
                        .pathParam(ID, initialTicket.getId())
                        .get(TICKET_BY_ID)
                        .then()
                        .log().body()
                        .statusCode(HttpStatus.FORBIDDEN.value())
                        .extract().response().as(ApiError.class);
                assertTrue(apiError.getDetail().startsWith("Current user with role"));
                assertTrue(apiError.getDetail().endsWith("from outer group can not read ticket if ticket is from another group"));
            }
        }
    }

    @Test
    @Order(25)
    void LimitAllInitialUsersOnAllTicketTypes() {
        itHelper.limitAllInitialUsersOnAllTicketTypes();
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllInitialUsersExceptObservers")
    @Order(30)
    void AccessDeniedWhenLimitAllInitialUsersOnAllTicketTypes(String userKey, User currentUser) {
        UUID idOfTicketWhichAuthorIdEqualsCurrentUserId = null;
        var initialTickets = itHelper.getTickets().values();
        for (TicketDtoResponse initialTicket : initialTickets) {
            if (initialTicket.getAuthor().getId().equals(currentUser.getId())) {
                idOfTicketWhichAuthorIdEqualsCurrentUserId = initialTicket.getId();
                break;
            }
        }
        var apiError = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .pathParam(ID, idOfTicketWhichAuthorIdEqualsCurrentUserId)
                .get(TICKET_BY_ID)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value())
                .extract().response().as(ApiError.class);
    }

    @Test
    @Order(35)
    void AllowAllInitialUsersOnAllTicketTypes() {
        itHelper.allowAllInitialUsersOnAllTicketTypes();
    }


    private static Stream<Arguments> getStreamAllInitialUsersExceptObservers() {
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

    private static Stream<Arguments> getStreamInitialUsersWithRolesAdminAndExecutorFromOuterGroup() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(
                        ADMIN,
                        EXECUTOR
                )
        );
        return itHelper.getStreamUsers(roles, false);
    }
}
