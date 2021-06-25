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
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.repository.UserRepository;
import ru.itterminal.yanmas.commons.exception.error.ApiError;
import ru.itterminal.yanmas.commons.model.dto.BaseEntityDto;
import ru.itterminal.yanmas.security.jwt.JwtProvider;
import ru.itterminal.yanmas.tickets.model.dto.TicketDtoResponse;
import ru.itterminal.yanmas.tickets.model.dto.TicketEventDtoRequest;
import ru.itterminal.yanmas.tickets.model.dto.TicketEventDtoResponse;
import ru.itterminal.yanmas.tickets.model.test.TicketTestHelper;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static io.restassured.RestAssured.given;
import static io.restassured.path.json.JsonPath.from;
import static java.lang.String.format;
import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.*;
import static ru.itterminal.yanmas.IT.util.ITHelper.*;
import static ru.itterminal.yanmas.tickets.service.business_handler.ticket_event.ReopenTicketOnCreationTicketEventIfTicketIsFinishedAndCurrentUserIsAuthorTicketBusinessHandler.USER_REOPEN_TICKET;
import static ru.itterminal.yanmas.tickets.service.validator.ticket.check_access_before_create_update.CurrentUserRoleAuthorCanNotCreateUpdateTicketIfAuthorOfTicketIsNotCurrentUserValidator.CURRENT_USER_WITH_ROLE_AUTHOR_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_NOT_CURRENT_USER;
import static ru.itterminal.yanmas.tickets.service.validator.ticket.logical_validation.ExecutorCannotBeAUserWeighingLessThanTheWeightOfTheExecutorValidator.WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS;
import static ru.itterminal.yanmas.tickets.service.validator.ticket_event.check_access_before_create.MustNotCreateTicketEventIfCurrentUserAreNonTicketTicketEventValidator.YOU_ARE_NOT_A_MEMBER_OF_THIS_TICKET;
import static ru.itterminal.yanmas.tickets.service.validator.ticket_event.logical_validation.MustNotCreateTicketEventIfRecipientsAreNonTicketTicketEventValidator.RECIPIENT_NOT_FOUND_IN_THE_TICKET;
import static ru.itterminal.yanmas.tickets.service.validator.ticket_event.logical_validation.MustNotCreateTicketEventIfRecipientsAreNonTicketTicketEventValidator.TICKET_EVENT_IS_INVALID;

@SuppressWarnings("unused")
@DataJpaTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class, JwtProvider.class, UserRepository.class})
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@TestPropertySource(properties = {"jwt.token.secret=ksedtob", "jwt.token.expired=8640000", "jwt.token.prefix=Bearer"})
class TicketEventCreateIT {

    @Autowired
    private UserRepository userRepository;

    private static final ITHelper itHelper = new ITHelper();
    private final TicketTestHelper ticketTestHelper = new TicketTestHelper();

    private User userWithoutTicket;

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
        userWithoutTicket = itHelper.createUserByGivenUserForGivenRoleAndGroupWithoutSaveInMaps(
                        itHelper.getAccountOwner().getGroup(),
                        itHelper.getRoleTestHelper().getRoleByName(Roles.ADMIN.toString()),
                        itHelper.getAccountOwner());
    }

    @ParameterizedTest(name = "{index} User: {0}    Ticket: {2}")
    @MethodSource("getStreamAllInitialUsersAllTickets")
    @Order(10)
    void successCreateEventsFromTicketIfCurrentUserIsInTicketAndHasAccessToTicket(String userKey, User currentUser,
                                                                                  String ticketKey, TicketDtoResponse ticket) {
        var comment = itHelper.getFaker().lorem().paragraph();
        var ticketEventDtoRequest = TicketEventDtoRequest.builder()
                .comment(comment)
                .build();
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketEventDtoRequest)
                .pathParam(ID, ticket.getId())
                .post(TICKET_BY_ID + EVENT)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(TicketEventDtoResponse.class);
        assertEquals(ticket.getId(), response.getTicketId());
        assertEquals(comment, response.getComment());
        assertEquals(currentUser.getId(), response.getCreatedBy().getId());
    }

    @ParameterizedTest(name = "{index} User: {0}    Ticket: {2}")
    @MethodSource("getStreamAllInitialUsersAllTickets")
    @Order(20)
    void successCreateEventsFromTicketIfCurrentUserIsInTicketAndHasAccessToTicketAndRecipientIsInTicket(String userKey, User currentUser,
                                                                                                        String ticketKey, TicketDtoResponse ticket) {

        var userInTicket = itHelper.getAllUserOfTicket(ticket);
        var recipients = userInTicket.stream()
                .filter(u -> !u.equals(currentUser.getId()))
                .collect(Collectors.toList());
        if (recipients.isEmpty()) {
            recipients.add(currentUser.getId());
        }
        var ticketEventDtoRequest = TicketEventDtoRequest.builder()
                .recipients(recipients)
                .comment(itHelper.getFaker().lorem().paragraph())
                .build();
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketEventDtoRequest)
                .pathParam(ID, ticket.getId())
                .post(TICKET_BY_ID + EVENT)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(TicketEventDtoResponse.class);
        var listUuidRecipients = response.getRecipients().stream()
                .map(BaseEntityDto::getId)
                .collect(Collectors.toList());
        assertEquals(userInTicket.size(), response.getRecipients().size());
        assertTrue(listUuidRecipients.contains(currentUser.getId()));
    }

    @ParameterizedTest(name = "{index} User: {0}    Ticket: {2}")
    @MethodSource("getStreamAllInitialUsersAllTickets")
    @Order(30)
    void failedCreateEventsFromTicketIfRecipientIsNotInTicket(String userKey, User currentUser,
                                                              String ticketKey, TicketDtoResponse ticket) {

        var ticketEventDtoRequest = TicketEventDtoRequest.builder()
                .comment(itHelper.getFaker().lorem().paragraph())
                .recipients(List.of(userWithoutTicket.getId()))
                .build();
        var apiError = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketEventDtoRequest)
                .pathParam(ID, ticket.getId())
                .post(TICKET_BY_ID + EVENT)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value())
                .extract().response().as(ApiError.class);
        var expectedMessage = format(RECIPIENT_NOT_FOUND_IN_THE_TICKET, userWithoutTicket.getDisplayName());
        var actualMessage = apiError.getErrors().get(TICKET_EVENT_IS_INVALID).get(0).getMessage();
        assertEquals(expectedMessage, actualMessage);
    }

    @ParameterizedTest(name = "{index} User: {0}    Ticket: {2}")
    @MethodSource("getStreamAllInitialUsersAllTicketsWithoutUser")
    @Order(40)
    void failedCreateEventsFromTicketIfCurrentUserIsNotInTicket(String userKey, User currentUser,
                                                              String ticketKey, TicketDtoResponse ticket) {

        var ticketEventDtoRequest = TicketEventDtoRequest.builder()
                .comment(itHelper.getFaker().lorem().paragraph())
                .build();
        var apiError = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketEventDtoRequest)
                .pathParam(ID, ticket.getId())
                .post(TICKET_BY_ID + EVENT)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @ParameterizedTest(name = "{index} User: {0}    Ticket: {2}")
    @MethodSource("getStreamAllInitialUsersWithOuterGroupAllTicketsIsNotOwnGroup")
    @Order(50)
    void failedCreateEventsFromTicketIfCurrentUserDoesNotHaveAccessToTicket(String userKey, User currentUser,
                                                                String ticketKey, TicketDtoResponse ticket) {

        var ticketEventDtoRequest = TicketEventDtoRequest.builder()
                .comment(itHelper.getFaker().lorem().paragraph())
                .build();
        var apiError = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketEventDtoRequest)
                .pathParam(ID, ticket.getId())
                .post(TICKET_BY_ID + EVENT)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Order(60)
    @Test
    void finishTicketBeforeReopenTest() {
        var updateTicket = itHelper.getTicketFromUser(itHelper.getAccountOwner());
        var updateTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(updateTicket, false);
        updateTicketDtoRequest.setIsFinished(true);
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(updateTicketDtoRequest)
                .put(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value());
    }

    @Order(70)
    @Test
    void successReopenTicketIfTicketIsFinishAndCurrentUserIsAuthorInTicket() {
        var ticket = itHelper.getTicketFromUser(itHelper.getAccountOwner());
        var ticketEventDtoRequest = TicketEventDtoRequest.builder()
                .comment(itHelper.getFaker().lorem().paragraph())
                .build();
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketEventDtoRequest)
                .pathParam(ID, ticket.getId())
                .post(TICKET_BY_ID + EVENT)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(TicketEventDtoResponse.class);
        assertEquals(format(USER_REOPEN_TICKET, itHelper.getAccountOwner().getDisplayName()), response.getAutoComment());


        var ticketDtoResponse = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .pathParam(ID, ticket.getId())
                .get(TICKET_BY_ID)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(TicketDtoResponse.class);
        assertFalse(ticketDtoResponse.getIsFinished());
        assertEquals(itHelper.getTicketStatuses().get(IS_REOPENED_PREDEFINED).getId(), ticketDtoResponse.getTicketStatus().getId());
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

    private static Stream<Arguments> getStreamAllInitialUsersAllTicketsWithoutUser() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(ACCOUNT_OWNER,
                        ADMIN,
                        EXECUTOR,
                        AUTHOR,
                        OBSERVER
                )
        );
        return itHelper.getStreamUsersAndTickets(roles, null, false, null);
    }

    private static Stream<Arguments> getStreamAllInitialUsersWithOuterGroupAllTicketsIsNotOwnGroup() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(ADMIN,
                        EXECUTOR,
                        AUTHOR,
                        OBSERVER
                )
        );
        return itHelper.getStreamUsersAndTickets(roles, false, null, false);
    }
}
