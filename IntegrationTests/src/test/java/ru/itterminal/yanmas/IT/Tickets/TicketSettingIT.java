package ru.itterminal.yanmas.IT.Tickets;

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
import ru.itterminal.yanmas.IT.util.ITHelper;
import ru.itterminal.yanmas.IT.util.ITTestConfig;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.repository.UserRepository;
import ru.itterminal.yanmas.commons.exception.error.ApiError;
import ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter;
import ru.itterminal.yanmas.security.jwt.JwtProvider;
import ru.itterminal.yanmas.tickets.model.TicketSetting;
import ru.itterminal.yanmas.tickets.model.dto.TicketSettingFilterDto;
import ru.itterminal.yanmas.tickets.model.test.TicketSettingTestHelper;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static io.restassured.RestAssured.given;
import static io.restassured.path.json.JsonPath.from;
import static java.lang.String.format;
import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.*;
import static ru.itterminal.yanmas.IT.util.ITHelper.*;
import static ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;
import static ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl.NOT_UNIQUE_CODE;
import static ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl.NOT_UNIQUE_MESSAGE;
import static ru.itterminal.yanmas.tickets.service.validator.ticket_setting.check_access_before_create.NotAllowedCreateFromCurrentUserIfTicketTypeIsNotPermitted.ACCESS_DENIED_BECAUSE_CURRENT_USER_HAS_NOT_PERMIT_TO_TICKET_TYPE;
import static ru.itterminal.yanmas.tickets.service.validator.ticket_setting.logical_validation.CheckAccessAuthorFromTicketSettingHasToTicketTypeValidator.INVALID_TICKET_SETTINGS;
import static ru.itterminal.yanmas.tickets.service.validator.ticket_setting.logical_validation.CheckAccessAuthorFromTicketSettingHasToTicketTypeValidator.INVALID_TICKET_SETTINGS_BECAUSE_AUTHOR_HAS_NOT_ACCESS_TO_TICKET_TYPE;
import static ru.itterminal.yanmas.tickets.service.validator.ticket_setting.logical_validation.CheckTicketSettingMustNotBeEmptyValidator.TICKET_SETTING_MUST_NOT_BE_EMPTY;
import static ru.itterminal.yanmas.tickets.service.validator.ticket_setting.logical_validation.CheckUniquesBeforeCreateTicketSettingValidator.TICKET_SETTING_UNIQUE_FIELDS;

@SuppressWarnings({"unused", "deprecation"})
@DataJpaTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class, JwtProvider.class})
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@TestPropertySource(properties = {"jwt.token.secret=ksedtob", "jwt.token.expired=8640000", "jwt.token.prefix=Bearer"})
class TicketSettingIT {

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
        itHelper.createInitialSettingsAccessToTicketTypes();
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersWithoutRoleObserver")
    @Order(10)
    void successGetByFilterByAllUsersWithoutRoleObserverWithEmptyFilter(String userKey, User user) {
        @SuppressWarnings("SimplifyStreamApiCallChains")
        var expectedTicketSettingList = itHelper.getTicketSettings().values().stream().collect(Collectors.toList());
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .param(SIZE, expectedTicketSettingList.size())
                .get(TICKET_SETTING)
                .then()
                .log().body()
                .body(TOTAL_ELEMENTS, equalTo(expectedTicketSettingList.size()))
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        var actualTicketSettingList = from(response).getList(CONTENT, TicketSetting.class);
        assertEquals(expectedTicketSettingList.size(), actualTicketSettingList.size());
        AssertionsForInterfaceTypes.assertThat(actualTicketSettingList)
                .usingElementComparatorIgnoringFields(IGNORE_FIELDS_FOR_COMPARE_TICKET_SETTING)
                .containsExactlyInAnyOrderElementsOf(expectedTicketSettingList);
        assertThat(actualTicketSettingList).usingRecursiveComparison().ignoringActualNullFields()
                .isEqualTo(expectedTicketSettingList);
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersWithoutRoleObserver")
    @Order(20)
    void successGetByFilterByAllUsersWithoutRoleObserverWithFilterFromGroupOfTicketSetting(String userKey, User user) {
        for (TicketSetting expectedTicketSetting : itHelper.getTicketSettings().values()) {
            var groupFilter = BaseEntityFilter.builder()
                    .typeComparison(EXIST_IN.toString())
                    .listOfIdEntities(List.of(expectedTicketSetting.getGroup().getId()))
                    .build();
            var filter = TicketSettingFilterDto.builder()
                    .group(groupFilter)
                    .build();
            var response = given().
                    when()
                    .headers(
                            "Authorization",
                            "Bearer " + itHelper.getTokens().get(user.getEmail())
                    )
                    .contentType(APPLICATION_JSON)
                    .body(filter)
                    .param(SIZE, 1)
                    .get(TICKET_SETTING)
                    .then()
                    .log().body()
                    .body(TOTAL_ELEMENTS, equalTo(1))
                    .statusCode(HttpStatus.OK.value())
                    .extract().response().asString();
            var actualTicketSettingList = from(response).getList(CONTENT, TicketSetting.class);
            var actualTicketSetting = actualTicketSettingList.get(0);
            assertThat(actualTicketSetting).usingRecursiveComparison().ignoringActualNullFields()
                    .isEqualTo(expectedTicketSetting);
        }
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersWithRoleObserver")
    @Order(25)
    void failedGetByFilterByAllUserWithRoleObserver(String userKey, User user) {
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(new TicketSettingFilterDto())
                .get(TICKET_SETTING)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(30)
    void failedGetByFilterByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .get(TICKET_SETTING)
                .then()
                .log().body()
                .statusCode(HttpStatus.UNAUTHORIZED.value());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersWithoutRoleObserver")
    @Order(40)
    void successGetByIdByAllUsersWithoutRoleObserver(String userKey, User user) {
        for (TicketSetting expectedTicketSetting : itHelper.getTicketSettings().values()) {
            var actualTicketSetting = given().
                    when().
                    headers(
                            "Authorization",
                            "Bearer " + itHelper.getTokens().get(user.getEmail())
                    )
                    .contentType(APPLICATION_JSON)
                    .pathParam(ID, expectedTicketSetting.getId())
                    .get(TICKET_SETTING_BY_ID)
                    .then()
                    .log().body()
                    .body(ID, equalTo(expectedTicketSetting.getId().toString()))
                    .statusCode(HttpStatus.OK.value())
                    .extract().response().as(TicketSetting.class);
            assertThat(actualTicketSetting).usingRecursiveComparison().ignoringActualNullFields()
                    .isEqualTo(expectedTicketSetting);
        }
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersWithRoleObserver")
    @Order(45)
    void failedGetByIdByAllUserWithRoleObserver(String userKey, User user) {
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .pathParam(ID, UUID.randomUUID().toString())
                .get(TICKET_SETTING_BY_ID)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(50)
    void failedGetByIdByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .pathParam(ID, UUID.randomUUID().toString())
                .get(TICKET_SETTING_BY_ID)
                .then()
                .log().body()
                .statusCode(HttpStatus.UNAUTHORIZED.value());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersFromInnerGroup_1AndFromOuterGroup_1")
    @Order(60)
    void successGetByAuthorByAllUsersFromInnerGroup_1AndFromOuterGroup_1WithoutRoleObserver(String userKey, User user) {
        if (!user.getRole().getName().equals(Roles.OBSERVER.toString())) {
            var expectedTicketSetting = user.getGroup().getIsInner()
                    ? itHelper.getTicketSettings().get(INNER_GROUP + "1")
                    : itHelper.getTicketSettings().get(OUTER_GROUP + "1");
            var actualTicketSetting = given().
                    when().
                    headers(
                            "Authorization",
                            "Bearer " + itHelper.getTokens().get(user.getEmail())
                    )
                    .contentType(APPLICATION_JSON)
                    .pathParam(AUTHOR_ID, user.getId())
                    .get(TICKET_SETTING_BY_AUTHOR)
                    .then()
                    .log().body()
                    .body(ID, equalTo(expectedTicketSetting.getId().toString()))
                    .statusCode(HttpStatus.OK.value())
                    .extract().response().as(TicketSetting.class);
            assertThat(actualTicketSetting).usingRecursiveComparison()
                    .ignoringActualNullFields()
                    .ignoringFields("ticketStatusForCancel", "ticketStatusForClose", "ticketStatusForNew",
                            "ticketStatusForReopen", "ticketTypeForNew"
                    )
                    .isEqualTo(expectedTicketSetting);
        }
    }

    @Test
    @Order(65)
    void successGetByAuthor_whereValueOfTicketTypeIsPredefined_ifAuthorOfTicketHasNotPermittedToTicketTypeFromTicketSettings() {
        var expectedTicketSetting = itHelper.getTicketSettings().get(INNER_GROUP + "1");
        var author = itHelper.getAuthorInnerGroup().get(AUTHOR_INNER_GROUP + "1");
        var authorId = author.getId();
        var actualTicketSetting = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(author.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .pathParam(AUTHOR_ID, authorId)
                .get(TICKET_SETTING_BY_AUTHOR)
                .then()
                .log().body()
                .body(ID, equalTo(expectedTicketSetting.getId().toString()))
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(TicketSetting.class);
        assertThat(actualTicketSetting).usingRecursiveComparison()
                .ignoringActualNullFields()
                .ignoringFields("ticketStatusForCancel", "ticketStatusForClose", "ticketStatusForNew",
                        "ticketStatusForReopen", "ticketTypeForNew"
                )
                .isEqualTo(expectedTicketSetting);
        var predefinedTicketType = itHelper.getTicketTypes().get(IT_IS_PREDEFINED_TICKET_TYPE_FOR_NEW_TICKET);
        assertEquals(predefinedTicketType.getId(), actualTicketSetting.getTicketTypeForNew().getId());
        assertNotEquals(expectedTicketSetting.getTicketTypeForNew().getId(),
                actualTicketSetting.getTicketTypeForNew().getId()
        );
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersFromInnerGroup_2AndFromOuterGroup_2")
    @Order(70)
    void successGetByAuthorByAllUsersFromInnerGroup_2AndFromOuterGroup_2WithoutRoleObserver(String userKey, User user) {
        if (!user.getRole().getName().equals(Roles.OBSERVER.toString())) {
            var actualTicketSetting = given().
                    when().
                    headers(
                            "Authorization",
                            "Bearer " + itHelper.getTokens().get(user.getEmail())
                    )
                    .contentType(APPLICATION_JSON)
                    .pathParam(AUTHOR_ID, user.getId())
                    .get(TICKET_SETTING_BY_AUTHOR)
                    .then()
                    .log().body()
                    .statusCode(HttpStatus.OK.value())
                    .extract().response().as(TicketSetting.class);
            assertNull(actualTicketSetting.getId());
            assertTrue(actualTicketSetting.getObservers().isEmpty());
            assertTrue(actualTicketSetting.getExecutors().isEmpty());
            assertNotNull(actualTicketSetting.getTicketTypeForNew());
            assertNotNull(actualTicketSetting.getTicketStatusForNew());
            assertNotNull(actualTicketSetting.getTicketStatusForClose());
            assertNotNull(actualTicketSetting.getTicketStatusForCancel());
            assertNotNull(actualTicketSetting.getTicketStatusForReopen());
        }
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersFromOuterGroupsWithoutRoleObserver")
    @Order(73)
    void failedGetByAuthorByAllUsersFromOuterGroupsWithoutRoleObserverButNotEqualGroups(String userKey, User user) {

        given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .pathParam(AUTHOR_ID, itHelper.getAdminInnerGroup().get(ADMIN_INNER_GROUP_1).getId())
                .get(TICKET_SETTING_BY_AUTHOR)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersWithRoleObserver")
    @Order(75)
    void failedGetByAuthorByAllUsersWithRoleObserver(String userKey, User user) {
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .pathParam(AUTHOR_ID, UUID.randomUUID().toString())
                .get(TICKET_SETTING_BY_AUTHOR)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(80)
    void failedGetByAuthorByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .pathParam(AUTHOR_ID, UUID.randomUUID().toString())
                .get(TICKET_SETTING_BY_AUTHOR)
                .then()
                .log().body()
                .statusCode(HttpStatus.UNAUTHORIZED.value());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(90)
    void successCreateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin(String userKey, User user) {
        var ticketType = itHelper.getTicketTypes().get(IT_IS_PREDEFINED_TICKET_TYPE_FOR_NEW_TICKET);
        var expectedTicketSetting = itHelper.getTicketSettings().get(INNER_GROUP + "1").toBuilder()
                .group(user.getGroup())
                .author(user)
                .ticketTypeForNew(ticketType)
                .deleted(null)
                .displayName(null)
                .id(null)
                .version(null)
                .build();
        var dtoRequest = ticketSettingTestHelper.convertEntityToDtoRequest(expectedTicketSetting, true);
        var actualTicketSetting = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(dtoRequest)
                .post(TICKET_SETTING)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(TicketSetting.class);
        assertThat(actualTicketSetting).usingRecursiveComparison()
                .ignoringActualNullFields()
                .ignoringFields(IGNORE_FIELDS_OF_BASE_ENTITY_FOR_COMPARE)
                .ignoringFields("ticketStatusForCancel", "ticketStatusForClose", "ticketStatusForNew",
                        "ticketStatusForReopen", "ticketTypeForNew"
                )
                .isEqualTo(expectedTicketSetting);
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(100)
    void failedCreateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin_whenTicketSettingIsEmpty
            (String userKey, User user) {
        var newTicketSetting = itHelper.getTicketSettings().get(INNER_GROUP + "1").toBuilder()
                .group(user.getGroup())
                .author(user)
                .ticketTypeForNew(null)
                .observers(null)
                .executors(null)
                .deleted(null)
                .displayName(null)
                .id(null)
                .version(null)
                .build();
        var dtoRequest = ticketSettingTestHelper.convertEntityToDtoRequest(newTicketSetting, true);
        var apiError = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(dtoRequest)
                .post(TICKET_SETTING)
                .then()
                .log().body()
                .body(STATUS, equalTo(HttpStatus.CONFLICT.value()))
                .extract().response().as(ApiError.class);
        assertEquals(
                TICKET_SETTING_MUST_NOT_BE_EMPTY,
                apiError.getErrors().get(INVALID_TICKET_SETTINGS).get(0).getMessage()
        );
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(110)
    void failedCreateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin_whenTicketSettingIsNotUnique
            (String userKey, User user) {
        var newTicketSetting = itHelper.getTicketSettings().get(INNER_GROUP + "1").toBuilder()
                .deleted(null)
                .displayName(null)
                .id(null)
                .version(null)
                .build();
        var dtoRequest = ticketSettingTestHelper.convertEntityToDtoRequest(newTicketSetting, true);
        var apiError = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(dtoRequest)
                .post(TICKET_SETTING)
                .then()
                .log().body()
                .body(STATUS, equalTo(HttpStatus.CONFLICT.value()))
                .extract().response().as(ApiError.class);
        assertEquals(
                format(NOT_UNIQUE_MESSAGE, TICKET_SETTING_UNIQUE_FIELDS),
                apiError.getErrors().get(NOT_UNIQUE_CODE).get(0).getMessage()
        );
    }

    @Test
    @Order(114)
    void failedCreateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin_whenAuthorHasNotPermittedToTicketType() {
        var author = itHelper.getAuthorInnerGroup().get(AUTHOR_INNER_GROUP + "1");
        var newTicketSetting = itHelper.getTicketSettings().get(INNER_GROUP + "1").toBuilder()
                .author(author)
                .deleted(null)
                .displayName(null)
                .id(null)
                .version(null)
                .build();
        var dtoRequest = ticketSettingTestHelper.convertEntityToDtoRequest(newTicketSetting, true);
        var apiError = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(dtoRequest)
                .post(TICKET_SETTING)
                .then()
                .log().body()
                .body(STATUS, equalTo(HttpStatus.CONFLICT.value()))
                .extract().response().as(ApiError.class);
        assertEquals(
                INVALID_TICKET_SETTINGS_BECAUSE_AUTHOR_HAS_NOT_ACCESS_TO_TICKET_TYPE,
                apiError.getErrors().get(INVALID_TICKET_SETTINGS).get(0).getMessage()
        );
    }

    @Test
    @Order(117)
    void failedCreateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin_whenCurrentUserHasNotPermittedToTicketType() {
        var currentUser = itHelper.getAdminInnerGroup().get(ADMIN_INNER_GROUP_1);
        var group = itHelper.getInnerGroup().get(INNER_GROUP_1);
        var groupTicketTypes = itHelper.getGroupTicketTypes().get(INITIAL_GROUP_TICKET_TYPES);
        itHelper.createSettingsAccessToTicketTypesWithoutAddingIntoSettingsAccessToTicketTypesMap(group, currentUser, groupTicketTypes);
        var author = itHelper.getAuthorInnerGroup().get(AUTHOR_INNER_GROUP + "2");
        var newTicketSetting = itHelper.getTicketSettings().get(INNER_GROUP + "1").toBuilder()
                .author(author)
                .deleted(null)
                .displayName(null)
                .id(null)
                .version(null)
                .build();
        var dtoRequest = ticketSettingTestHelper.convertEntityToDtoRequest(newTicketSetting, true);
        var apiError = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(dtoRequest)
                .post(TICKET_SETTING)
                .then()
                .log().body()
                .body(STATUS, equalTo(HttpStatus.FORBIDDEN.value()))
                .extract().response().as(ApiError.class);
        assertEquals(
                ACCESS_DENIED_BECAUSE_CURRENT_USER_HAS_NOT_PERMIT_TO_TICKET_TYPE,
                apiError.getDetail()
        );
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(120)
    void failedCreateByAllUsers_whenUserIsNotInnerGroupAndNotHaveRoleAccountOwnerOrAdmin
            (String userKey, User user) {
        if (user.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString()) ||
                (user.getRole().getName().equals(Roles.ADMIN.toString()) && user.getGroup().getIsInner())) {
            return;
        }
        var newTicketSetting = itHelper.getTicketSettings().get(INNER_GROUP + "1").toBuilder()
                .deleted(null)
                .displayName(null)
                .id(null)
                .version(null)
                .build();
        var dtoRequest = ticketSettingTestHelper.convertEntityToDtoRequest(newTicketSetting, true);
        given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(dtoRequest)
                .post(TICKET_SETTING)
                .then()
                .log().body()
                .body(STATUS, equalTo(HttpStatus.FORBIDDEN.value()));
    }

    @Test
    @Order(130)
    void failedCreateByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .post(TICKET_SETTING)
                .then()
                .log().body()
                .statusCode(HttpStatus.UNAUTHORIZED.value());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(140)
    void successUpdateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin(String userKey, User user) {
        var ticketType = itHelper.getTicketTypes().get(IT_IS_PREDEFINED_TICKET_TYPE_FOR_NEW_TICKET);
        var expectedTicketSetting = itHelper.getTicketSettings().get(INNER_GROUP + "1").toBuilder()
                .ticketTypeForNew(ticketType)
                .displayName(null)
                .build();
        var dtoRequest = ticketSettingTestHelper.convertEntityToDtoRequest(expectedTicketSetting, false);
        var actualTicketSetting = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(dtoRequest)
                .put(TICKET_SETTING)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(TicketSetting.class);
        itHelper.getTicketSettings().get(INNER_GROUP + "1").setVersion(actualTicketSetting.getVersion());
        assertThat(actualTicketSetting).usingRecursiveComparison()
                .ignoringActualNullFields()
                .ignoringFields(IGNORE_FIELDS_OF_BASE_ENTITY_FOR_COMPARE)
                .ignoringFields("ticketStatusForCancel", "ticketStatusForClose", "ticketStatusForNew",
                        "ticketStatusForReopen", "ticketTypeForNew"
                )
                .isEqualTo(expectedTicketSetting);
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(150)
    void failedUpdateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin_whenAfterUpdateTicketSettingWillBeEmpty(
            String userKey, User user) {
        var expectedTicketSetting = itHelper.getTicketSettings().get(INNER_GROUP + "1").toBuilder()
                .ticketTypeForNew(null)
                .observers(null)
                .executors(null)
                .displayName(null)
                .build();
        var dtoRequest = ticketSettingTestHelper.convertEntityToDtoRequest(expectedTicketSetting, false);
        var apiError = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(dtoRequest)
                .put(TICKET_SETTING)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value())
                .extract().response().as(ApiError.class);
        assertEquals(
                TICKET_SETTING_MUST_NOT_BE_EMPTY,
                apiError.getErrors().get(INVALID_TICKET_SETTINGS).get(0).getMessage()
        );
    }

    @Test
    @Order(160)
    void failedUpdateByUsersFromInnerGroupsWithRolesAccountOwner_whenAfterUpdateTicketSettingWillBeNotUnique() {
        var expectedTicketSetting = itHelper.getTicketSettings().get(INNER_GROUP + "1").toBuilder()
                .group(itHelper.getTicketSettings().get(OUTER_GROUP + "1").getGroup())
                .displayName(null)
                .build();
        var dtoRequest = ticketSettingTestHelper.convertEntityToDtoRequest(expectedTicketSetting, false);
        var apiError = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(dtoRequest)
                .put(TICKET_SETTING)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value())
                .extract().response().as(ApiError.class);
        assertEquals(
                format(NOT_UNIQUE_MESSAGE, TICKET_SETTING_UNIQUE_FIELDS),
                apiError.getErrors().get(NOT_UNIQUE_CODE).get(0).getMessage()
        );
    }

    @Test
    @Order(164)
    void failedUpdateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin_whenAuthorHasNotPermittedToTicketType() {
        var author = itHelper.getAuthorInnerGroup().get(AUTHOR_INNER_GROUP + "1");
        var newTicketSetting = itHelper.getTicketSettings().get(INNER_GROUP + "1").toBuilder()
                .author(author)
                .displayName(null)
                .build();
        var dtoRequest = ticketSettingTestHelper.convertEntityToDtoRequest(newTicketSetting, true);
        var apiError = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(dtoRequest)
                .put(TICKET_SETTING)
                .then()
                .log().body()
                .body(STATUS, equalTo(HttpStatus.CONFLICT.value()))
                .extract().response().as(ApiError.class);
        assertEquals(
                INVALID_TICKET_SETTINGS_BECAUSE_AUTHOR_HAS_NOT_ACCESS_TO_TICKET_TYPE,
                apiError.getErrors().get(INVALID_TICKET_SETTINGS).get(0).getMessage()
        );
    }

    @Test
    @Order(167)
    void failedUpdateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin_whenCurrentUserHasNotPermittedToTicketType() {
        var currentUser = itHelper.getAdminInnerGroup().get(ADMIN_INNER_GROUP_1);
        var group = itHelper.getInnerGroup().get(INNER_GROUP_1);
        var groupTicketTypes = itHelper.getGroupTicketTypes().get(INITIAL_GROUP_TICKET_TYPES);
        var author = itHelper.getAuthorInnerGroup().get(AUTHOR_INNER_GROUP + "2");
        var newTicketSetting = itHelper.getTicketSettings().get(INNER_GROUP + "1").toBuilder()
                .author(author)
                .displayName(null)
                .build();
        var dtoRequest = ticketSettingTestHelper.convertEntityToDtoRequest(newTicketSetting, true);
        var apiError = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(dtoRequest)
                .put(TICKET_SETTING)
                .then()
                .log().body()
                .body(STATUS, equalTo(HttpStatus.FORBIDDEN.value()))
                .extract().response().as(ApiError.class);
        assertEquals(
                ACCESS_DENIED_BECAUSE_CURRENT_USER_HAS_NOT_PERMIT_TO_TICKET_TYPE,
                apiError.getDetail()
        );
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(170)
    void failedUpdateByAllUsers_whenUserIsNotInnerGroupAndNotHaveRoleAccountOwnerOrAdmin
            (String userKey, User user) {
        if (user.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString()) ||
                (user.getRole().getName().equals(Roles.ADMIN.toString()) && user.getGroup().getIsInner())) {
            return;
        }
        var expectedTicketSetting = itHelper.getTicketSettings().get(INNER_GROUP + "1").toBuilder()
                .displayName(null)
                .build();
        var dtoRequest = ticketSettingTestHelper.convertEntityToDtoRequest(expectedTicketSetting, false);
        given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(dtoRequest)
                .put(TICKET_SETTING)
                .then()
                .log().body()
                .body(STATUS, equalTo(HttpStatus.FORBIDDEN.value()));
    }

    @Test
    @Order(180)
    void failedUpdateByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .put(TICKET_SETTING)
                .then()
                .log().body()
                .statusCode(HttpStatus.UNAUTHORIZED.value());
    }

    private static Stream<Arguments> getStreamAllUsersFromInnerGroup_2AndFromOuterGroup_2() {
        return itHelper.getStreamAllUsersFromGroups(
                List.of(
                        itHelper.getInnerGroup().get(INNER_GROUP + "2"),
                        itHelper.getOuterGroup().get(OUTER_GROUP + "2")
                )
        );
    }

    private static Stream<Arguments> getStreamAllUsersFromInnerGroup_1AndFromOuterGroup_1() {
        return itHelper.getStreamAllUsersFromGroups(
                List.of(
                        itHelper.getTicketSettings().get(INNER_GROUP + "1").getGroup(),
                        itHelper.getTicketSettings().get(OUTER_GROUP + "1").getGroup()
                )
        );
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

    private static Stream<Arguments> getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin() {
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

    private static Stream<Arguments> getStreamAllUsersWithoutRoleObserver() {
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

    private static Stream<Arguments> getStreamAllUsersFromOuterGroupsWithoutRoleObserver() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(
                        ADMIN,
                        EXECUTOR,
                        AUTHOR
                )
        );
        return itHelper.getStreamUsers(roles, false);
    }

    private static Stream<Arguments> getStreamAllUsersWithRoleObserver() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(List.of(OBSERVER));
        return itHelper.getStreamUsers(roles, null);
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
