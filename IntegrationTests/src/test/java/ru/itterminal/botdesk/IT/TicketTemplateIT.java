package ru.itterminal.botdesk.IT;

import io.restassured.RestAssured;
import org.assertj.core.api.AssertionsForInterfaceTypes;
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
import ru.itterminal.botdesk.tickets.model.TicketTemplate;
import ru.itterminal.botdesk.tickets.model.test.TicketTemplateTestHelper;

import java.util.List;
import java.util.stream.Stream;

import static io.restassured.RestAssured.given;
import static io.restassured.path.json.JsonPath.from;
import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.collection.IsMapContaining.hasKey;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static ru.itterminal.botdesk.IT.util.ITHelper.*;

@DataJpaTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class, JwtProvider.class})
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@TestPropertySource(properties = {"jwt.token.secret=ksedtob", "jwt.token.expired=8640000", "jwt.token.prefix=Bearer"})
class TicketTemplateIT {

    public static final String EXPRESSION_SCHEDULE = "expressionSchedule";
    public static final String ZONE_ID = "zoneId";
    @Autowired
    private UserRepository userRepository;

    private static final ITHelper itHelper = new ITHelper();
    private final TicketTemplateTestHelper ticketTemplateTestHelper = new TicketTemplateTestHelper();

    @BeforeAll
    void beforeAll() {
        RestAssured.useRelaxedHTTPSValidation();
        RestAssured.baseURI = "https://localhost:8080";
        RestAssured.basePath = "/api/v1/";
        itHelper.createAndVerifyAccount(userRepository);
        itHelper.createInitialInnerAndOuterGroups(1, 2);
        itHelper.createInitialUsers();
        itHelper.createInitialTicketTemplates();
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAdminExecutor")
    @Order(10)
    void successGetByFilterByUsersInnerGroupWithRolesAccountOwnerAdminExecutor(String userKey, User user) {
        var expectedTicketTemplateList = itHelper.getTicketTemplates().values();
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .param(SIZE, expectedTicketTemplateList.size())
                .get(TICKET_TEMPLATE)
                .then()
                .log().body()
                .body(TOTAL_ELEMENTS, equalTo(expectedTicketTemplateList.size()))
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        List<TicketTemplate> actualTicketTemplateList = from(response).getList(CONTENT, TicketTemplate.class);
        assertEquals(expectedTicketTemplateList.size(), actualTicketTemplateList.size());
        AssertionsForInterfaceTypes.assertThat(actualTicketTemplateList)
                .usingElementComparatorIgnoringFields(IGNORE_FIELDS_FOR_COMPARE_TICKET_TEMPLATE)
                .containsExactlyInAnyOrderElementsOf(expectedTicketTemplateList);
        org.assertj.core.api.Assertions.assertThat(actualTicketTemplateList).usingRecursiveComparison().ignoringActualNullFields()
                .isEqualTo(expectedTicketTemplateList);
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAuthorObserver")
    @Order(20)
    void failedGetByFilterByUsersInnerGroupWithRolesAuthorObserver(String userKey, User user) {
        given()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .when()
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .get(TICKET_TEMPLATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersOuterGroup")
    @Order(30)
    void failedGetByFilterByUsersOuterGroup(String userKey, User user) {
        given()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .when()
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .get(TICKET_TEMPLATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }


    @Test
    @Order(40)
    void failedGetByFilterByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .get(TICKET_TEMPLATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAdminExecutor")
    @Order(50)
    void successGetByIdByUsersInnerGroupWithRolesAccountOwnerAdminExecutor(String userKey, User user) {
        var ticketTemplateList = itHelper.getTicketTemplates().values();
        for (TicketTemplate expectedTicketTemplate : ticketTemplateList) {
            var actualTicketTemplate = given().
                    when()
                    .headers(
                            "Authorization",
                            "Bearer " + itHelper.getTokens().get(user.getEmail())
                    )
                    .pathParam(ID, expectedTicketTemplate.getId())
                    .get(TICKET_TEMPLATE_BY_ID)
                    .then()
                    .log().body()
                    .statusCode(HttpStatus.OK.value())
                    .extract().response().as(TicketTemplate.class);
            assertThat(actualTicketTemplate).usingRecursiveComparison().ignoringActualNullFields()
                    .isEqualTo(expectedTicketTemplate);
        }
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAuthorObserver")
    @Order(60)
    void failedGetByIdByUsersInnerGroupWithRolesAuthorObserver(String userKey, User user) {
        var ticketTemplateList = itHelper.getTicketTemplates().values();
        for (TicketTemplate expectedTicketTemplate : ticketTemplateList) {
            given().
                    when()
                    .headers(
                            "Authorization",
                            "Bearer " + itHelper.getTokens().get(user.getEmail())
                    )
                    .pathParam(ID, expectedTicketTemplate.getId())
                    .get(TICKET_TEMPLATE_BY_ID)
                    .then()
                    .log().body()
                    .statusCode(HttpStatus.FORBIDDEN.value());
        }
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersOuterGroup")
    @Order(70)
    void failedGetByIdByUsersOuterGroup(String userKey, User user) {
        var ticketTemplateList = itHelper.getTicketTemplates().values();
        for (TicketTemplate expectedTicketTemplate : ticketTemplateList) {
            given().
                    when()
                    .headers(
                            "Authorization",
                            "Bearer " + itHelper.getTokens().get(user.getEmail())
                    )
                    .pathParam(ID, expectedTicketTemplate.getId())
                    .get(TICKET_TEMPLATE_BY_ID)
                    .then()
                    .log().body()
                    .statusCode(HttpStatus.FORBIDDEN.value());
        }
    }

    @Test
    @Order(80)
    void failedGetByIdByAnonymousUser() {
        var ticketTemplateList = itHelper.getTicketTemplates().values();
        for (TicketTemplate expectedTicketTemplate : ticketTemplateList) {
            given().
                    when()
                    .pathParam(ID, expectedTicketTemplate.getId())
                    .get(TICKET_TEMPLATE_BY_ID)
                    .then()
                    .log().body()
                    .statusCode(HttpStatus.FORBIDDEN.value());
        }
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAdminExecutor")
    @Order(90)
    void successCreateByUsersInnerGroupWithRolesAccountOwnerAdminExecutor(String userKey, User user) {
        var expectedTicketTemplate = ticketTemplateTestHelper.getRandomValidEntity();
        expectedTicketTemplate.setTicketType(itHelper.getTicketTypes().get(IS_PREDEFINED_FOR_NEW_TICKET));
        expectedTicketTemplate.setAuthor(itHelper.getAuthorOuterGroup().get(AUTHOR_OUTER_GROUP + 1));
        var ticketTemplateDtoRequest = ticketTemplateTestHelper.convertEntityToDtoRequest(expectedTicketTemplate, true);
        var actualTicketTemplate = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketTemplateDtoRequest)
                .post(TICKET_TEMPLATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(TicketTemplate.class);
        assertThat(actualTicketTemplate).usingRecursiveComparison()
                .ignoringActualNullFields()
                .ignoringFields("id", "version", "deleted", "displayName", "dateNextRun")
                .isEqualTo(expectedTicketTemplate);
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAuthorObserver")
    @Order(100)
    void failedCreateByUsersInnerGroupWithRolesAuthorObserver(String userKey, User user) {
        var expectedTicketTemplate = ticketTemplateTestHelper.getRandomValidEntity();
        var ticketTemplateDtoRequest = ticketTemplateTestHelper.convertEntityToDtoRequest(expectedTicketTemplate, true);
        ticketTemplateDtoRequest.setTicketTypeId(itHelper.getTicketTypes().get(IS_PREDEFINED_FOR_NEW_TICKET).getId());
        ticketTemplateDtoRequest.setAuthorId(itHelper.getAuthorOuterGroup().get(AUTHOR_OUTER_GROUP + 1).getId());
        var actualTicketTemplate = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketTemplateDtoRequest)
                .post(TICKET_TEMPLATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersOuterGroup")
    @Order(110)
    void failedCreateByUsersOuterGroup(String userKey, User user) {
        var expectedTicketTemplate = ticketTemplateTestHelper.getRandomValidEntity();
        var ticketTemplateDtoRequest = ticketTemplateTestHelper.convertEntityToDtoRequest(expectedTicketTemplate, true);
        ticketTemplateDtoRequest.setTicketTypeId(itHelper.getTicketTypes().get(IS_PREDEFINED_FOR_NEW_TICKET).getId());
        ticketTemplateDtoRequest.setAuthorId(itHelper.getAuthorOuterGroup().get(AUTHOR_OUTER_GROUP + 1).getId());
        var actualTicketTemplate = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketTemplateDtoRequest)
                .post(TICKET_TEMPLATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(120)
    void failedCreateByByAnonymousUser() {
        var expectedTicketTemplate = ticketTemplateTestHelper.getRandomValidEntity();
        var ticketTemplateDtoRequest = ticketTemplateTestHelper.convertEntityToDtoRequest(expectedTicketTemplate, true);
        ticketTemplateDtoRequest.setTicketTypeId(itHelper.getTicketTypes().get(IS_PREDEFINED_FOR_NEW_TICKET).getId());
        ticketTemplateDtoRequest.setAuthorId(itHelper.getAuthorOuterGroup().get(AUTHOR_OUTER_GROUP + 1).getId());
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(ticketTemplateDtoRequest)
                .post(TICKET_TEMPLATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(130)
    void failedCreateWhenExpressionScheduleAndZoneIdNotFormat() {
        var expectedTicketTemplate = ticketTemplateTestHelper.getRandomValidEntity();
        expectedTicketTemplate.setTicketType(itHelper.getTicketTypes().get(IS_PREDEFINED_FOR_NEW_TICKET));
        expectedTicketTemplate.setAuthor(itHelper.getAuthorOuterGroup().get(AUTHOR_OUTER_GROUP + 1));
        expectedTicketTemplate.setExpressionSchedule("2565322*");
        expectedTicketTemplate.setZoneId("+7");
        var ticketTemplateDtoRequest = ticketTemplateTestHelper.convertEntityToDtoRequest(expectedTicketTemplate, true);

        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketTemplateDtoRequest)
                .post(TICKET_TEMPLATE)
                .then()
                .body(ERRORS, hasKey(EXPRESSION_SCHEDULE))
                .body(ERRORS, hasKey(ZONE_ID))
                .log().body()
                .statusCode(HttpStatus.BAD_REQUEST.value());
    }

    @Test
    @Order(130)
    void failedCreateWhenExpressionScheduleNotValid() {
        var expectedTicketTemplate = ticketTemplateTestHelper.getRandomValidEntity();
        expectedTicketTemplate.setTicketType(itHelper.getTicketTypes().get(IS_PREDEFINED_FOR_NEW_TICKET));
        expectedTicketTemplate.setAuthor(itHelper.getAuthorOuterGroup().get(AUTHOR_OUTER_GROUP + 1));
        expectedTicketTemplate.setExpressionSchedule("25 6 5 32 2 *");
        var ticketTemplateDtoRequest = ticketTemplateTestHelper.convertEntityToDtoRequest(expectedTicketTemplate, true);

        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketTemplateDtoRequest)
                .post(TICKET_TEMPLATE)
                .then()
                .body(ERRORS, hasKey(EXPRESSION_SCHEDULE))
                .log().body()
                .statusCode(HttpStatus.BAD_REQUEST.value());
    }


    ////////

    @SuppressWarnings({"unused", "deprecation"})
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAdminExecutor")
    @Order(140)
    void successUpdateByUsersInnerGroupWithRolesAccountOwnerAdminExecutor(String userKey, User user) {
        var expectedTicketTemplate =itHelper.getTicketTemplates().get(TICKET_TEMPLATE_KEY + 1);
        expectedTicketTemplate.setSubject(user.getEmail());
        var ticketTemplateDtoRequest = ticketTemplateTestHelper.convertEntityToDtoRequest(expectedTicketTemplate, false);
        var actualTicketTemplate = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketTemplateDtoRequest)
                .put(TICKET_TEMPLATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(TicketTemplate.class);
        assertThat(actualTicketTemplate).usingRecursiveComparison()
                .ignoringActualNullFields()
                .ignoringFields("displayName", "dateNextRun", "version")
                .isEqualTo(expectedTicketTemplate);
        assertEquals(actualTicketTemplate.getVersion(), expectedTicketTemplate.getVersion() + 1);
        expectedTicketTemplate.setVersion(actualTicketTemplate.getVersion());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAuthorObserver")
    @Order(150)
    void failedUpdateByUsersInnerGroupWithRolesAuthorObserver(String userKey, User user) {
        var expectedTicketTemplate = ticketTemplateTestHelper.getRandomValidEntity();
        var ticketTemplateDtoRequest = ticketTemplateTestHelper.convertEntityToDtoRequest(expectedTicketTemplate, false);
        ticketTemplateDtoRequest.setTicketTypeId(itHelper.getTicketTypes().get(IS_PREDEFINED_FOR_NEW_TICKET).getId());
        ticketTemplateDtoRequest.setAuthorId(itHelper.getAuthorOuterGroup().get(AUTHOR_OUTER_GROUP + 1).getId());
        var actualTicketTemplate = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketTemplateDtoRequest)
                .put(TICKET_TEMPLATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersOuterGroup")
    @Order(160)
    void failedUpdateByUsersOuterGroup(String userKey, User user) {
        var expectedTicketTemplate = ticketTemplateTestHelper.getRandomValidEntity();
        var ticketTemplateDtoRequest = ticketTemplateTestHelper.convertEntityToDtoRequest(expectedTicketTemplate, false);
        ticketTemplateDtoRequest.setTicketTypeId(itHelper.getTicketTypes().get(IS_PREDEFINED_FOR_NEW_TICKET).getId());
        ticketTemplateDtoRequest.setAuthorId(itHelper.getAuthorOuterGroup().get(AUTHOR_OUTER_GROUP + 1).getId());
        var actualTicketTemplate = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketTemplateDtoRequest)
                .put(TICKET_TEMPLATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(170)
    void failedUpdateByByAnonymousUser() {
        var expectedTicketTemplate = ticketTemplateTestHelper.getRandomValidEntity();
        var ticketTemplateDtoRequest = ticketTemplateTestHelper.convertEntityToDtoRequest(expectedTicketTemplate, false);
        ticketTemplateDtoRequest.setTicketTypeId(itHelper.getTicketTypes().get(IS_PREDEFINED_FOR_NEW_TICKET).getId());
        ticketTemplateDtoRequest.setAuthorId(itHelper.getAuthorOuterGroup().get(AUTHOR_OUTER_GROUP + 1).getId());
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(ticketTemplateDtoRequest)
                .put(TICKET_TEMPLATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(180)
    void failedUpdateWhenExpressionScheduleAndZoneIdNotFormat() {
        var expectedTicketTemplate = ticketTemplateTestHelper.getRandomValidEntity();
        expectedTicketTemplate.setTicketType(itHelper.getTicketTypes().get(IS_PREDEFINED_FOR_NEW_TICKET));
        expectedTicketTemplate.setAuthor(itHelper.getAuthorOuterGroup().get(AUTHOR_OUTER_GROUP + 1));
        expectedTicketTemplate.setExpressionSchedule("2565322*");
        expectedTicketTemplate.setZoneId("+7");
        var ticketTemplateDtoRequest = ticketTemplateTestHelper.convertEntityToDtoRequest(expectedTicketTemplate, true);

        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketTemplateDtoRequest)
                .put(TICKET_TEMPLATE)
                .then()
                .body(ERRORS, hasKey(EXPRESSION_SCHEDULE))
                .body(ERRORS, hasKey(ZONE_ID))
                .log().body()
                .statusCode(HttpStatus.BAD_REQUEST.value());
    }

    @Test
    @Order(190)
    void failedUpdateWhenExpressionScheduleNotValid() {
        var expectedTicketTemplate = ticketTemplateTestHelper.getRandomValidEntity();
        expectedTicketTemplate.setTicketType(itHelper.getTicketTypes().get(IS_PREDEFINED_FOR_NEW_TICKET));
        expectedTicketTemplate.setAuthor(itHelper.getAuthorOuterGroup().get(AUTHOR_OUTER_GROUP + 1));
        expectedTicketTemplate.setExpressionSchedule("25 6 5 32 2 *");
        var ticketTemplateDtoRequest = ticketTemplateTestHelper.convertEntityToDtoRequest(expectedTicketTemplate, true);

        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketTemplateDtoRequest)
                .put(TICKET_TEMPLATE)
                .then()
                .body(ERRORS, hasKey(EXPRESSION_SCHEDULE))
                .log().body()
                .statusCode(HttpStatus.BAD_REQUEST.value());
    }

    private static Stream<Arguments> getStreamUsersInnerGroupWithRolesAccountOwnerAdminExecutor() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(ACCOUNT_OWNER,
                        ADMIN,
                        EXECUTOR
                )
        );
        return itHelper.getStreamUsers(roles, true);
    }

    private static Stream<Arguments> getStreamUsersInnerGroupWithRolesAuthorObserver() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(AUTHOR,
                        OBSERVER
                )
        );
        return itHelper.getStreamUsers(roles, true);
    }

    private static Stream<Arguments> getStreamUsersOuterGroup() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(ACCOUNT_OWNER,
                        ADMIN,
                        EXECUTOR,
                        AUTHOR,
                        OBSERVER
                )
        );
        return itHelper.getStreamUsers(roles, false);
    }


}
