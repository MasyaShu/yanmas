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
import ru.itterminal.yanmas.commons.model.dto.BaseEntityDto;
import ru.itterminal.yanmas.security.jwt.JwtProvider;
import ru.itterminal.yanmas.tickets.model.dto.TicketDtoResponse;
import ru.itterminal.yanmas.tickets.model.dto.TicketFilterDto;
import ru.itterminal.yanmas.tickets.model.test.TicketTestHelper;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static io.restassured.RestAssured.given;
import static io.restassured.path.json.JsonPath.from;
import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static ru.itterminal.yanmas.IT.util.ITHelper.*;

@DataJpaTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class, JwtProvider.class, UserRepository.class})
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@TestPropertySource(properties = {"jwt.token.secret=ksedtob", "jwt.token.expired=8640000", "jwt.token.prefix=Bearer"})
class TicketFindByFilterIT {

    @Autowired
    private UserRepository userRepository;

    private static final ITHelper itHelper = new ITHelper();
    private final TicketTestHelper ticketTestHelper = new TicketTestHelper();

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

    @Test
    @Order(10)
    void SuccessWhenCurrentUserIsAccountOwnerAndFilterIsEmpty() {
        var actualTicketsList = itHelper.getTickets().values();
        var expectedCountOfInitialTickets = actualTicketsList.size();
        var listOfTicketDtoResponse = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .get(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        List<TicketDtoResponse> expectedTicketList = from(listOfTicketDtoResponse).getList(CONTENT, TicketDtoResponse.class);
        var actualCountOfInitialTickets = expectedTicketList.size();
        assertEquals(expectedCountOfInitialTickets, actualCountOfInitialTickets);
        assertThat(expectedTicketList).usingElementComparatorOnFields("id").containsAll(actualTicketsList);
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllInitialUsersWithRoleAuthor")
    @Order(20)
    void SuccessWhenCurrentUserIsAuthorOfTicketAndFilterIsEmpty(String userKey, User currentUser) {
        var listOfTicketDtoResponse = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .get(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        List<TicketDtoResponse> expectedTicketList = from(listOfTicketDtoResponse).getList(CONTENT, TicketDtoResponse.class);
        assertEquals(1, expectedTicketList.size());
        var idOfAuthorOfFoundTicket = expectedTicketList.get(0).getAuthor().getId();
        assertEquals(idOfAuthorOfFoundTicket, currentUser.getId());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamInitialUsersWithRolesAdminAndExecutorFromOuterGroup")
    @Order(30)
    void SuccessWhenCurrentUserHasRoleAdminOrExecutorFromOuterGroupAndFilterIsEmpty(String userKey, User currentUser) {
        var listOfTicketDtoResponse = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .get(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        List<TicketDtoResponse> expectedTickets = from(listOfTicketDtoResponse).getList(CONTENT, TicketDtoResponse.class);
        var expectedGroupsIdOfExpectedTickets = expectedTickets.stream()
                .map(item -> item.getGroup().getId())
                .distinct()
                .collect(Collectors.toList());
        assertEquals(1, expectedGroupsIdOfExpectedTickets.size());
        assertEquals(currentUser.getGroup().getId(), expectedGroupsIdOfExpectedTickets.get(0));
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamInitialUsersWithRoleObserverFromInnerGroup")
    @Order(40)
    void SuccessWhenCurrentUserIsFromObserversOfInitialTicketAndFilterIsEmpty(String userKey, User currentUser) {
        var listOfTicketDtoResponse = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .get(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        List<TicketDtoResponse> expectedTickets = from(listOfTicketDtoResponse).getList(CONTENT, TicketDtoResponse.class);
        var expectedTicketsWhichHaveCurrentUserAsObserver = expectedTickets.stream()
                .filter(item -> {
                    var observersId = item.getObservers().stream()
                            .map(BaseEntityDto::getId)
                            .collect(Collectors.toList());
                    return observersId.contains(currentUser.getId());
                })
                .collect(Collectors.toList());
        assertEquals(expectedTickets.size(), expectedTicketsWhichHaveCurrentUserAsObserver.size());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllInitialUsersExceptObservers")
    @Order(50)
    void SuccessWhenCurrentUserIsAuthorOfTicketAndFilterIsNotEmpty(String userKey, User currentUser) {
        TicketFilterDto filter = null;
        List<TicketDtoResponse> actualTicketsList = new ArrayList<>();
        var initialTickets = itHelper.getTickets().values();
        for (TicketDtoResponse initialTicket : initialTickets) {
            if (initialTicket.getAuthor().getId().equals(currentUser.getId())) {
                filter = ticketTestHelper.convertTicketDtoResponseToTicketFilterDto(initialTicket);
                actualTicketsList.add(initialTicket);
                break;
            }
        }
        var listOfTicketDtoResponse = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(filter)
                .get(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        List<TicketDtoResponse> expectedTicketList = from(listOfTicketDtoResponse).getList(CONTENT, TicketDtoResponse.class);
        assertEquals(1, expectedTicketList.size());
        assertThat(expectedTicketList).usingElementComparatorOnFields("id").containsAll(actualTicketsList);
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamInitialUsersWithRoleObserverFromOuterGroup")
    @Order(60)
    void SuccessWhenCurrentUserWithRoleObserverAndHeIsNotObserverOfTicket(String userKey, User currentUser) {
        var initialTickets = itHelper.getTickets().values();
        boolean isCurrentUserFromObserversOfInitialTicket = false;
        for (TicketDtoResponse initialTicket : initialTickets) {
            var observers = initialTicket.getObservers().stream()
                    .map(BaseEntityDto::getId)
                    .collect(Collectors.toList());
            if (observers.contains(currentUser.getId())) {
                isCurrentUserFromObserversOfInitialTicket = true;
                break;
            }
        }
        if (!isCurrentUserFromObserversOfInitialTicket) {
            var listOfTicketDtoResponse = given().
                    when().
                    headers(
                            "Authorization",
                            "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                    )
                    .contentType(APPLICATION_JSON)
                    .body(EMPTY_BODY)
                    .get(TICKET)
                    .then()
                    .log().body()
                    .statusCode(HttpStatus.OK.value())
                    .extract().response().asString();
            List<TicketDtoResponse> expectedTicketList = from(listOfTicketDtoResponse).getList(CONTENT, TicketDtoResponse.class);
            assertEquals(0, expectedTicketList.size());
        }
    }

    @SuppressWarnings("unused")
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

    @SuppressWarnings("unused")
    private static Stream<Arguments> getStreamAllInitialUsersWithRoleAuthor() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(
                        AUTHOR
                )
        );
        return itHelper.getStreamUsers(roles, null);
    }

    @SuppressWarnings("unused")
    private static Stream<Arguments> getStreamInitialUsersWithRolesAdminAndExecutorFromOuterGroup() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(
                        ADMIN,
                        EXECUTOR
                )
        );
        return itHelper.getStreamUsers(roles, false);
    }

    @SuppressWarnings("unused")
    private static Stream<Arguments> getStreamInitialUsersWithRoleObserverFromInnerGroup() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(
                        OBSERVER
                )
        );
        return itHelper.getStreamUsers(roles, true);
    }

    @SuppressWarnings("unused")
    private static Stream<Arguments> getStreamInitialUsersWithRoleObserverFromOuterGroup() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(
                        OBSERVER
                )
        );
        return itHelper.getStreamUsers(roles, false);
    }
}
