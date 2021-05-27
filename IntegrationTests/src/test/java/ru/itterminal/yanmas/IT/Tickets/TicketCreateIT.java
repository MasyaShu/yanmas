package ru.itterminal.yanmas.IT.Tickets;

import io.restassured.RestAssured;
import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;
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
import ru.itterminal.yanmas.tickets.model.Priority;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.model.dto.TicketDtoResponse;
import ru.itterminal.yanmas.tickets.model.test.TicketSettingTestHelper;
import ru.itterminal.yanmas.tickets.model.test.TicketTestHelper;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static io.restassured.RestAssured.given;
import static java.lang.String.format;
import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static ru.itterminal.yanmas.IT.util.ITHelper.*;
import static ru.itterminal.yanmas.aau.service.validator.EntityValidator.EMPTY_TICKET;
import static ru.itterminal.yanmas.tickets.service.validator.ticket.check_access_before_create_update.CurrentUserRoleAuthorCanNotCreateUpdateTicketIfAuthorOfTicketIsNotCurrentUserValidator.CURRENT_USER_WITH_ROLE_AUTHOR_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_NOT_CURRENT_USER;
import static ru.itterminal.yanmas.tickets.service.validator.ticket.logical_validation.AuthorCannotBeAUserWeighingLessThanTheWeightOfTheAuthorValidator.WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR;
import static ru.itterminal.yanmas.tickets.service.validator.ticket.logical_validation.AuthorCannotBeAUserWeighingLessThanTheWeightOfTheAuthorValidator.WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR_LESS_THAN_WEIGHT_OF_ROLE_AUTHOR;
import static ru.itterminal.yanmas.tickets.service.validator.ticket.logical_validation.CheckAuthorTicketHasAccessToTicketTypeValidator.INVALID_TICKET;
import static ru.itterminal.yanmas.tickets.service.validator.ticket.logical_validation.CheckAuthorTicketHasAccessToTicketTypeValidator.INVALID_TICKET_BECAUSE_AUTHOR_HAS_NOT_ACCESS_TO_TICKET_TYPE;
import static ru.itterminal.yanmas.tickets.service.validator.ticket.logical_validation.ExecutorCannotBeAUserWeighingLessThanTheWeightOfTheExecutorValidator.WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS;
import static ru.itterminal.yanmas.tickets.service.validator.ticket.logical_validation.ExecutorCannotBeAUserWeighingLessThanTheWeightOfTheExecutorValidator.WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS_LESS_THAN_WEIGHT_OF_ROLE_EXECUTOR;
import static ru.itterminal.yanmas.tickets.service.validator.ticket.logical_validation.ExecutorFromOuterGroupCanBeIfHisGroupEqualsGroupOfTicketValidator.EXECUTOR_FROM_OUTER_GROUP_CAN_BE_IF_HIS_GROUP_EQUALS_GROUP_OF_TICKET;
import static ru.itterminal.yanmas.tickets.service.validator.ticket.logical_validation.ExecutorFromOuterGroupCanBeIfHisGroupEqualsGroupOfTicketValidator.EXECUTOR_IS_INVALID;
import static ru.itterminal.yanmas.tickets.service.validator.ticket.logical_validation.MustNotCreateUpdateTicketIfSubjectDescriptionFilesAreEmptyValidator.MUST_NOT_CREATE_UPDATE_TICKET_IF_SUBJECT_DESCRIPTION_AND_FILES_ARE_EMPTY;
import static ru.itterminal.yanmas.tickets.service.validator.ticket.logical_validation.ObserverFromOuterGroupCanBeIfHisGroupEqualsGroupOfTicketValidator.OBSERVER_FROM_OUTER_GROUP_CAN_BE_IF_HIS_GROUP_EQUALS_GROUP_OF_TICKET;
import static ru.itterminal.yanmas.tickets.service.validator.ticket.logical_validation.ObserverFromOuterGroupCanBeIfHisGroupEqualsGroupOfTicketValidator.OBSERVER_IS_INVALID;

@SuppressWarnings({"unused"})
@DataJpaTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class, JwtProvider.class, UserRepository.class})
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@TestPropertySource(properties = {"jwt.token.secret=ksedtob", "jwt.token.expired=8640000", "jwt.token.prefix=Bearer"})
class TicketCreateIT {

    public static final String ACCOUNT_GROUP_AUTHOR_IS_OCCUPIED = "Account, Group, Author is occupied";
    public static final String ACCOUNT_GROUP_AUTHOR = "Account, Group, Author";
    public static final String ADMIN_INNER_GROUP_1 = "adminInnerGroup_1";

    @Autowired
    private UserRepository userRepository;

    private static final ITHelper itHelper = new ITHelper();
    private final TicketTestHelper ticketTestHelper = new TicketTestHelper();
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
        itHelper.createTicketTypeWhichIsNeverUsedIntoInitialTickets();
        itHelper.createGroupOfTicketTypesWhichIsNeverUsedIntoInitialTickets();
    }

    // CurrentUserRoleAuthorCanNotCreateUpdateTicketIfAuthorOfTicketIsNotCurrentUserValidator
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersWithRoleAuthor")
    @Order(10)
    void accessDenied_CurrentUserRoleAuthorCanNotCreateTicketIfAuthorOfTicketIsNotCurrentUser(String userKey, User currentUser) {
        var executorOfAuthorGroup = itHelper.getUsersByGroupAndRole(currentUser.getGroup(), null, itHelper.getRoleTestHelper().getRoleByName(EXECUTOR), null);
        var newTicket = Ticket.builder()
                .author(executorOfAuthorGroup.get(0))
                .subject(TICKET)
                .build();
        var newTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(newTicket, true);
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(newTicketDtoRequest)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value())
                .body("detail", equalTo(CURRENT_USER_WITH_ROLE_AUTHOR_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_NOT_CURRENT_USER));
    }

    //TicketSettingOperationValidator
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFromOuterGroupWithRoleAdminAndExecutor")
    @Order(20)
    void accessDenied_CurrentUserRoleAdminOrExecutorOuterGroupCanNotCreateTicketIfTicketIsFromAnotherGroup(String userKey, User currentUser) {
        var executorOfAuthorGroup = itHelper.getUsersByGroupAndRole(null, currentUser.getGroup(), itHelper.getRoleTestHelper().getRoleByName(AUTHOR), null);
        var newTicket = Ticket.builder()
                .author(executorOfAuthorGroup.get(0))
                .subject(TICKET)
                .build();
        var newTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(newTicket, true);
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(newTicketDtoRequest)
                .post(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    //SecurityConfig
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersWithRoleObserver")
    @Order(30)
    void accessDenied_accessDeniedIfCurrentUserByRoleObserver(String userKey, User currentUser) {
        var executorOfAuthorGroup = itHelper.getUsersByGroupAndRole(null, currentUser.getGroup(), itHelper.getRoleTestHelper().getRoleByName(AUTHOR), null);
        var newTicket = Ticket.builder()
                .author(executorOfAuthorGroup.get(0))
                .subject(TICKET)
                .build();
        var newTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(newTicket, true);
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(newTicketDtoRequest)
                .post(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    //AuthorCannotBeAUserWeighingLessThanTheWeightOfTheAuthorValidator
    @Test
    @Order(40)
    void logicalError_AuthorCannotBeAUserWeighingLessThanTheWeightOfTheAuthor() {
        var weightOfRoleAuthor = Roles.AUTHOR.getWeight();
        var AuthorTicket = itHelper.getObserverInnerGroup().get(OBSERVER_INNER_GROUP + 1);
        var newTicket = Ticket.builder()
                .author(AuthorTicket)
                .subject(TICKET)
                .build();
        var newTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(newTicket, true);
        var apiError = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(newTicketDtoRequest)
                .post(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value())
                .extract().response().as(ApiError.class);

        var expectedMessage = format(WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR_LESS_THAN_WEIGHT_OF_ROLE_AUTHOR,
                AuthorTicket.getRole().getWeight(),
                weightOfRoleAuthor);
        Assertions.assertEquals(expectedMessage, apiError.getErrors().get(WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR).get(0).getMessage());
    }

    //ExecutorCannotBeAUserWeighingLessThanTheWeightOfTheExecutorValidator
    @Test
    @Order(41)
    void logicalError_ExecutorCannotBeAUserWeighingLessThanTheWeightOfTheExecutor() {
        var weightOfRoleExecutor = Roles.EXECUTOR.getWeight();
        var executor = itHelper.getAuthorInnerGroup().get(AUTHOR_INNER_GROUP + 1);
        var newTicket = Ticket.builder()
                .author(itHelper.getAccountOwner())
                .subject(TICKET)
                .executors(List.of(executor))
                .build();
        var newTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(newTicket, true);
        var apiError = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(newTicketDtoRequest)
                .post(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value())
                .extract().response().as(ApiError.class);

        var expectedMessage = format(WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS_LESS_THAN_WEIGHT_OF_ROLE_EXECUTOR,
                executor.getRole().getWeight(),
                weightOfRoleExecutor);
        Assertions.assertEquals(expectedMessage, apiError.getErrors().get(WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS).get(0).getMessage());
    }

    //CheckAuthorTicketHasAccessToTicketTypeValidator
    @Test
    @Order(45)
    void LimitAllInitialUsersOnAllTicketTypes() {
        itHelper.limitAllInitialUsersOnAllTicketTypes();
    }

    @Test
    @Order(50)
    void logicalError_AuthorTicketHasAccessToTicketType() {
        var AuthorTicket = itHelper.getAuthorInnerGroup().get(AUTHOR_INNER_GROUP + 1);
        var newTicket = Ticket.builder()
                .author(AuthorTicket)
                .subject(TICKET)
                .build();
        var newTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(newTicket, true);
        var apiError = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(newTicketDtoRequest)
                .post(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value())
                .extract().response().as(ApiError.class);

        Assertions.assertEquals(INVALID_TICKET_BECAUSE_AUTHOR_HAS_NOT_ACCESS_TO_TICKET_TYPE,
                apiError.getErrors().get(INVALID_TICKET).get(0).getMessage());
    }

    @Test
    @Order(55)
    void AllowAllInitialUsersOnAllTicketTypes() {
        itHelper.allowAllInitialUsersOnAllTicketTypes();
    }

    //ExecutorFromOuterGroupCanBeIfHisGroupEqualsGroupOfTicketValidator
    @Test
    @Order(70)
    void logicalError_ExecutorFromOuterGroupCanBeIfHisGroupEqualsGroupOfTicket() {
        var executor = itHelper.getExecutorOuterGroup().get(EXECUTOR_OUTER_GROUP + 1);
        var author = itHelper.getAuthorOuterGroup().get(AUTHOR_OUTER_GROUP + 2);
        var newTicket = Ticket.builder()
                .author(author)
                .subject(TICKET)
                .executors(List.of(executor))
                .build();
        var newTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(newTicket, true);
        var apiError = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(newTicketDtoRequest)
                .post(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value())
                .extract().response().as(ApiError.class);

        Assertions.assertEquals(EXECUTOR_FROM_OUTER_GROUP_CAN_BE_IF_HIS_GROUP_EQUALS_GROUP_OF_TICKET, apiError.getErrors().get(EXECUTOR_IS_INVALID).get(0).getMessage());
    }

    //ObserverFromOuterGroupCanBeIfHisGroupEqualsGroupOfTicketValidator
    @Test
    @Order(80)
    void logicalError_ObserverFromOuterGroupCanBeIfHisGroupEqualsGroupOfTicket() {
        var observer = itHelper.getObserverOuterGroup().get(OBSERVER_OUTER_GROUP + 1);
        var author = itHelper.getAuthorOuterGroup().get(AUTHOR_OUTER_GROUP + 2);
        var newTicket = Ticket.builder()
                .author(author)
                .subject(TICKET)
                .observers(List.of(observer))
                .build();
        var newTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(newTicket, true);
        var apiError = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(newTicketDtoRequest)
                .post(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value())
                .extract().response().as(ApiError.class);

        Assertions.assertEquals(OBSERVER_FROM_OUTER_GROUP_CAN_BE_IF_HIS_GROUP_EQUALS_GROUP_OF_TICKET, apiError.getErrors().get(OBSERVER_IS_INVALID).get(0).getMessage());
    }

    //MustNotCreateUpdateTicketIfSubjectDescriptionFilesAreEmptyValidator
    @ParameterizedTest
    @NullAndEmptySource
    @Order(90)
    void logicalError_MustNotCreateTicketIfSubjectDescriptionFilesAreEmpty(String input) {
        var author = itHelper.getAuthorOuterGroup().get(AUTHOR_OUTER_GROUP + 2);
        var newTicket = Ticket.builder()
                .author(author)
                .subject(input)
                .subject(input)
                .build();
        if (input !=null) {
            newTicket.setFiles(List.of());
        }
        var newTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(newTicket, true);
        var apiError = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(newTicketDtoRequest)
                .post(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value())
                .extract().response().as(ApiError.class);

        Assertions.assertEquals(MUST_NOT_CREATE_UPDATE_TICKET_IF_SUBJECT_DESCRIPTION_AND_FILES_ARE_EMPTY, apiError.getErrors().get(EMPTY_TICKET).get(0).getMessage());
    }

    //SettingGroupFromAuthorOfTicketBeforeCreateAndUpdateTicketBusinessHandler
    //SettingTicketPriorityBeforeCreateAndUpdateTicketBusinessHandler
    //SettingTicketNumberBeforeCreateTicketBusinessHandler
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersWithRoleAccountOwnerAdminExecutorAuthor")
    @Order(100)
    void successCreate_CurrentUserByRoleAccountOwnerOrAdminOrExecutorOrAuthorSetGroupPriorotyNumberCreatedAt(String userKey, User currentUser) {
        var newTicket = Ticket.builder()
                .author(currentUser)
                .subject(TICKET)
                .build();
        var newTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(newTicket, true);
        var ticketDtoResponse = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(newTicketDtoRequest)
                .post(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(TicketDtoResponse.class);
        Assertions.assertEquals(currentUser.getGroup().getId(), ticketDtoResponse.getGroup().getId());
        Assertions.assertEquals(Priority.MIDDLE.toString(), ticketDtoResponse.getPriority());
        Assertions.assertTrue(ticketDtoResponse.getNumber() > 0);
        Assertions.assertTrue(ticketDtoResponse.getCreatedAt() <= System.currentTimeMillis());
    }

    //SettingTicketPriorityBeforeCreateAndUpdateTicketBusinessHandler
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersWithRoleAccountOwnerAdminExecutorAuthor")
    @Order(110)
    void successCreate_CurrentUserByRoleAccountOwnerOrAdminOrExecutorOrAuthorAndPriorityNotNull(String userKey, User currentUser) {
        var newTicket = Ticket.builder()
                .author(currentUser)
                .subject(TICKET)
                .priority(Priority.HEIGHT.toString())
                .build();
        var newTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(newTicket, true);
        var ticketDtoResponse = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(newTicketDtoRequest)
                .post(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(TicketDtoResponse.class);
        Assertions.assertEquals(currentUser.getGroup().getId(), ticketDtoResponse.getGroup().getId());
        Assertions.assertEquals(Priority.HEIGHT.toString(), ticketDtoResponse.getPriority());

    }

    //SettingTicketExecutorsBeforeCreateAndUpdateTicketBusinessHandler
    //SettingTicketObserversBeforeCreateAndUpdateTicketBusinessHandler
    //SettingTicketTypeBeforeCreateAndUpdateTicketBusinessHandler
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersWithRoleAccountOwnerAdminExecutorAuthor")
    @Order(120)
    void successCreate_ticketTypeExecutorsObserversFromTicketSetting(String userKey, User currentUser) {
        var ticketSettings = itHelper.getTicketSettingForUser(currentUser);
        var newTicket = Ticket.builder()
                .author(currentUser)
                .subject(TICKET)
                .build();
        var newTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(newTicket, true);
        var ticketDtoResponse = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(newTicketDtoRequest)
                .post(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(TicketDtoResponse.class);
        Assertions.assertEquals(ticketSettings.getTicketTypeForNew().getId(), ticketDtoResponse.getTicketType().getId());
        Assertions.assertEquals(ticketSettings.getObservers().size(), ticketDtoResponse.getObservers().size());
        Assertions.assertEquals(ticketSettings.getExecutors().size(), ticketDtoResponse.getExecutors().size());
        var observersIdExpected = ticketSettings.getObservers().stream()
                .map(User::getId)
                .collect(Collectors.toList());
        var observersIdActual = ticketDtoResponse.getObservers().stream()
                .map(BaseEntityDto::getId)
                .collect(Collectors.toList());
        assertThat(observersIdExpected)
                .containsExactlyInAnyOrderElementsOf(observersIdActual);

        var executorsIdExpected = ticketSettings.getExecutors().stream()
                .map(User::getId)
                .collect(Collectors.toList());
        var executorsIdActual = ticketDtoResponse.getExecutors().stream()
                .map(BaseEntityDto::getId)
                .collect(Collectors.toList());
        assertThat(executorsIdExpected)
                .containsExactlyInAnyOrderElementsOf(executorsIdActual);
    }

    //SettingTicketExecutorsBeforeCreateAndUpdateTicketBusinessHandler
    //SettingTicketObserversBeforeCreateAndUpdateTicketBusinessHandler
    //SettingTicketTypeBeforeCreateAndUpdateTicketBusinessHandler
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersWithRoleAuthor")
    @Order(130)
    void successCreate_ticketTypeExecutorsObserversFromTicketSettingWhenCurrentUserWithRoleAuthor(String userKey, User currentUser) {
        var ticketSettings = itHelper.getTicketSettingForUser(currentUser);
        var executors = itHelper.getUsersByGroupAndRole(currentUser.getGroup(), null,
                itHelper.getRoleTestHelper().getRoleByName(EXECUTOR), null);
        var observers = itHelper.getUsersByGroupAndRole(currentUser.getGroup(), null,
                itHelper.getRoleTestHelper().getRoleByName(OBSERVER), null);
        var newTicket = Ticket.builder()
                .author(currentUser)
                .subject(TICKET)
                .ticketType(itHelper.getTicketTypes().get(TICKET_TYPE_WHICH_IS_NEVER_USED_INTO_INITIAL_TICKETS))
                .build();
        var newTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(newTicket, true);
        var ticketDtoResponse = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(newTicketDtoRequest)
                .post(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(TicketDtoResponse.class);
        Assertions.assertEquals(ticketSettings.getTicketTypeForNew().getId(), ticketDtoResponse.getTicketType().getId());
        Assertions.assertEquals(ticketSettings.getObservers().size(), ticketDtoResponse.getObservers().size());
        Assertions.assertEquals(ticketSettings.getExecutors().size(), ticketDtoResponse.getExecutors().size());
        var observersIdExpected = ticketSettings.getObservers().stream()
                .map(User::getId)
                .collect(Collectors.toList());
        var observersIdActual = ticketDtoResponse.getObservers().stream()
                .map(BaseEntityDto::getId)
                .collect(Collectors.toList());
        assertThat(observersIdExpected)
                .containsExactlyInAnyOrderElementsOf(observersIdActual);

        var executorsIdExpected = ticketSettings.getExecutors().stream()
                .map(User::getId)
                .collect(Collectors.toList());
        var executorsIdActual = ticketDtoResponse.getExecutors().stream()
                .map(BaseEntityDto::getId)
                .collect(Collectors.toList());
        assertThat(executorsIdExpected)
                .containsExactlyInAnyOrderElementsOf(executorsIdActual);
    }

    //SettingTicketExecutorsBeforeCreateAndUpdateTicketBusinessHandler
    //SettingTicketObserversBeforeCreateAndUpdateTicketBusinessHandler
    //SettingTicketTypeBeforeCreateAndUpdateTicketBusinessHandler
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFromOuterGroupWithRoleAdminAndExecutorAndAuthor")
    @Order(140)
    void successCreate_ticketTypeExecutorsObserversFromTicketSettingWhenCurrentUserFromOuterGroup(String userKey, User currentUser) {
        var ticketSettings = itHelper.getTicketSettingForUser(currentUser);
        var executors = itHelper.getUsersByGroupAndRole(currentUser.getGroup(), null,
                itHelper.getRoleTestHelper().getRoleByName(EXECUTOR), null);
        var observers = itHelper.getUsersByGroupAndRole(currentUser.getGroup(), null,
                itHelper.getRoleTestHelper().getRoleByName(OBSERVER), null);
        var newTicket = Ticket.builder()
                .author(currentUser)
                .subject(TICKET)
                .ticketType(itHelper.getTicketTypes().get(TICKET_TYPE_WHICH_IS_NEVER_USED_INTO_INITIAL_TICKETS))
                .build();
        var newTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(newTicket, true);
        var ticketDtoResponse = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(newTicketDtoRequest)
                .post(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(TicketDtoResponse.class);
        Assertions.assertEquals(ticketSettings.getTicketTypeForNew().getId(), ticketDtoResponse.getTicketType().getId());
        Assertions.assertEquals(ticketSettings.getObservers().size(), ticketDtoResponse.getObservers().size());
        Assertions.assertEquals(ticketSettings.getExecutors().size(), ticketDtoResponse.getExecutors().size());
        var observersIdExpected = ticketSettings.getObservers().stream()
                .map(User::getId)
                .collect(Collectors.toList());
        var observersIdActual = ticketDtoResponse.getObservers().stream()
                .map(BaseEntityDto::getId)
                .collect(Collectors.toList());
        assertThat(observersIdExpected)
                .containsExactlyInAnyOrderElementsOf(observersIdActual);

        var executorsIdExpected = ticketSettings.getExecutors().stream()
                .map(User::getId)
                .collect(Collectors.toList());
        var executorsIdActual = ticketDtoResponse.getExecutors().stream()
                .map(BaseEntityDto::getId)
                .collect(Collectors.toList());
        assertThat(executorsIdExpected)
                .containsExactlyInAnyOrderElementsOf(executorsIdActual);
    }

    //SettingTicketStatusBeforeCreateAndUpdateTicketBusinessHandler
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersWithRoleAccountOwnerAdminExecutor")
    @Order(150)
    void successCreate_ticketStatusToFinishWhenCurrentUserWithRoleAccountOwnerAdminExecutorAndIsFinishedTrue(String userKey, User currentUser) {
        var ticketSettings = itHelper.getTicketSettingForUser(currentUser);
        var newTicket = Ticket.builder()
                .author(currentUser)
                .subject(TICKET)
                .isFinished(true)
                .build();
        var newTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(newTicket, true);
        var ticketDtoResponse = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(newTicketDtoRequest)
                .post(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(TicketDtoResponse.class);
        Assertions.assertEquals(ticketSettings.getTicketStatusForClose().getId(), ticketDtoResponse.getTicketStatus().getId());
        Assertions.assertTrue(ticketDtoResponse.getIsFinished());
    }

    //SettingTicketStatusBeforeCreateAndUpdateTicketBusinessHandler
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersWithRoleAuthor")
    @Order(160)
    void successCreate_ticketStatusFromTicketSettingWhenCurrentUserWithRoleAuthorAndIsFinishedTrue(String userKey, User currentUser) {
        var ticketSettings = itHelper.getTicketSettingForUser(currentUser);
        var newTicket = Ticket.builder()
                .author(currentUser)
                .subject(TICKET)
                .ticketStatus(ticketSettings.getTicketStatusForCancel())
                .isFinished(true)
                .build();
        var newTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(newTicket, true);
        var ticketDtoResponse = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(newTicketDtoRequest)
                .post(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(TicketDtoResponse.class);
        Assertions.assertEquals(ticketSettings.getTicketStatusForNew().getId(), ticketDtoResponse.getTicketStatus().getId());
        Assertions.assertFalse(ticketDtoResponse.getIsFinished());
    }

    //SettingTicketStatusBeforeCreateAndUpdateTicketBusinessHandler
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersWithRoleAccountOwnerAdminExecutor")
    @Order(170)
    void successCreate_ticketStatusFromRequestWhenCurrentUserWithRoleAccountOwnerAdminExecutor(String userKey, User currentUser) {
        var ticketSettings = itHelper.getTicketSettingForUser(currentUser);
        var newTicket = Ticket.builder()
                .author(currentUser)
                .subject(TICKET)
                .ticketStatus(ticketSettings.getTicketStatusForCancel())
                .build();
        var newTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(newTicket, true);
        var ticketDtoResponse = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(newTicketDtoRequest)
                .post(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(TicketDtoResponse.class);
        Assertions.assertEquals(ticketSettings.getTicketStatusForCancel().getId(), ticketDtoResponse.getTicketStatus().getId());
    }

    private static Stream<Arguments> getStreamAllUsersWithRoleAuthor() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(AUTHOR)
        );
        return itHelper.getStreamUsers(roles, null);
    }

    private static Stream<Arguments> getStreamAllUsersWithRoleAccountOwnerAdminExecutorAuthor() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(AUTHOR, EXECUTOR, ADMIN, ACCOUNT_OWNER)
        );
        return itHelper.getStreamUsers(roles, null);
    }

    private static Stream<Arguments> getStreamAllUsersWithRoleAccountOwnerAdminExecutor() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(EXECUTOR, ADMIN, ACCOUNT_OWNER)
        );
        return itHelper.getStreamUsers(roles, null);
    }

    private static Stream<Arguments> getStreamUsersWithRoleObserver() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(OBSERVER)
        );
        return itHelper.getStreamUsers(roles, null);
    }

    private static Stream<Arguments> getStreamUsersFromOuterGroupWithRoleAdminAndExecutor() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(ADMIN, EXECUTOR)
        );
        return itHelper.getStreamUsers(roles, false);
    }

    private static Stream<Arguments> getStreamUsersFromOuterGroupWithRoleAdminAndExecutorAndAuthor() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(ADMIN, EXECUTOR, AUTHOR)
        );
        return itHelper.getStreamUsers(roles, false);
    }

}
