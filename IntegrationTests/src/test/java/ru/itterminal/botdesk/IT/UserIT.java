package ru.itterminal.botdesk.IT;

import static io.restassured.RestAssured.given;
import static io.restassured.path.json.JsonPath.from;
import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static ru.itterminal.botdesk.IT.util.ITHelper.ACCOUNT;
import static ru.itterminal.botdesk.IT.util.ITHelper.ACCOUNT_OWNER;
import static ru.itterminal.botdesk.IT.util.ITHelper.ADMIN;
import static ru.itterminal.botdesk.IT.util.ITHelper.APPLICATION_JSON;
import static ru.itterminal.botdesk.IT.util.ITHelper.AUTHOR;
import static ru.itterminal.botdesk.IT.util.ITHelper.EMPTY_BODY;
import static ru.itterminal.botdesk.IT.util.ITHelper.EXECUTOR;
import static ru.itterminal.botdesk.IT.util.ITHelper.GROUP;
import static ru.itterminal.botdesk.IT.util.ITHelper.ID;
import static ru.itterminal.botdesk.IT.util.ITHelper.IGNORE_FIELDS_FOR_COMPARE_USERS;
import static ru.itterminal.botdesk.IT.util.ITHelper.INNER_GROUP;
import static ru.itterminal.botdesk.IT.util.ITHelper.OBSERVER;
import static ru.itterminal.botdesk.IT.util.ITHelper.OUTER_GROUP;
import static ru.itterminal.botdesk.IT.util.ITHelper.ROLE;
import static ru.itterminal.botdesk.IT.util.ITHelper.SIZE;
import static ru.itterminal.botdesk.IT.util.ITHelper.USER;
import static ru.itterminal.botdesk.IT.util.ITHelper.USER_BY_ID;

import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
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
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.aau.repository.UserRepository;
import ru.itterminal.botdesk.security.jwt.JwtProvider;

@SuppressWarnings("unused")
@DataJpaTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class, JwtProvider.class})
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@TestPropertySource(properties = {"jwt.token.secret=ksedtob", "jwt.token.expired=8640000", "jwt.token.prefix=Bearer"})
class UserIT {

    public static final String TOTAL_ELEMENTS = "totalElements";
    public static final String CONTENT = "content";
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
        itHelper.createUsersForEachRoleInGroup(
                itHelper.getOuterGroup().get(OUTER_GROUP + 2), allRolesWithoutAccountOwner);
        itHelper.createUsersForEachRoleInGroup(
                itHelper.getInnerGroup().get(INNER_GROUP + 2), allRolesWithoutAccountOwner);
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(10)
    void successGetByFilterByAllUsersViaFilterFromYourself(String userKey, User user) {
        var userFilterDto = userTestHelper.convertEntityToFilterDto(user);
        var expectedUserList = List.of(user);
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(userFilterDto)
                .param(SIZE, expectedUserList.size())
                .get(USER)
                .then()
                .log().body()
                .body(TOTAL_ELEMENTS, equalTo(expectedUserList.size()))
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        List<User> actualUserList = from(response).getList(CONTENT, User.class);
        assertThat(expectedUserList)
                .usingElementComparatorIgnoringFields(IGNORE_FIELDS_FOR_COMPARE_USERS)
                .containsExactlyInAnyOrderElementsOf(actualUserList);
        assertThat(expectedUserList).usingElementComparatorOnFields(ACCOUNT).usingElementComparatorOnFields(ID)
                .containsExactlyInAnyOrderElementsOf(actualUserList);
        assertThat(expectedUserList).usingElementComparatorOnFields(GROUP).usingElementComparatorOnFields(ID)
                .containsExactlyInAnyOrderElementsOf(actualUserList);
        assertThat(expectedUserList).usingElementComparatorOnFields(ROLE).usingElementComparatorOnFields(ID)
                .containsExactlyInAnyOrderElementsOf(actualUserList);
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersInnerGroup")
    @Order(20)
    void successGetByFilterByAllUsersFromInnerGroups(String userKey, User user) {
        var expectedUserList = itHelper.getAllUsers();
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .param(SIZE, expectedUserList.size())
                .get(USER)
                .then()
                .log().body()
                .body(TOTAL_ELEMENTS, equalTo(expectedUserList.size()))
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        List<User> actualUserList = from(response).getList(CONTENT, User.class);
        assertThat(expectedUserList)
                .usingElementComparatorIgnoringFields(IGNORE_FIELDS_FOR_COMPARE_USERS)
                .containsExactlyInAnyOrderElementsOf(actualUserList);
        assertThat(expectedUserList).usingElementComparatorOnFields(ACCOUNT).usingElementComparatorOnFields(ID)
                .containsExactlyInAnyOrderElementsOf(actualUserList);
        assertThat(expectedUserList).usingElementComparatorOnFields(GROUP).usingElementComparatorOnFields(ID)
                .containsExactlyInAnyOrderElementsOf(actualUserList);
        assertThat(expectedUserList).usingElementComparatorOnFields(ROLE).usingElementComparatorOnFields(ID)
                .containsExactlyInAnyOrderElementsOf(actualUserList);
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersOuterGroup")
    @Order(30)
    void successGetByFilterByAllUsersFromOuterGroups(String userKey, User user) {
        var expectedUserList = itHelper.getAllUsers().stream()
                .filter(u -> u.getGroup().equals(user.getGroup()))
                .collect(Collectors.toList());
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .param(SIZE, expectedUserList.size())
                .get(USER)
                .then()
                .log().body()
                .body(TOTAL_ELEMENTS, equalTo(expectedUserList.size()))
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        List<User> actualUserList = from(response).getList(CONTENT, User.class);
        assertThat(expectedUserList)
                .usingElementComparatorIgnoringFields(IGNORE_FIELDS_FOR_COMPARE_USERS)
                .containsExactlyInAnyOrderElementsOf(actualUserList);
        assertThat(expectedUserList).usingElementComparatorOnFields(ACCOUNT).usingElementComparatorOnFields(ID)
                .containsExactlyInAnyOrderElementsOf(actualUserList);
        assertThat(expectedUserList).usingElementComparatorOnFields(GROUP).usingElementComparatorOnFields(ID)
                .containsExactlyInAnyOrderElementsOf(actualUserList);
        assertThat(expectedUserList).usingElementComparatorOnFields(ROLE).usingElementComparatorOnFields(ID)
                .containsExactlyInAnyOrderElementsOf(actualUserList);
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(40)
    void successGetByFilterByAllUsersAccordingTheirGroups(String userKey, User user) {
        var allUsers = itHelper.getAllUsers();
        for (User one : allUsers) {
            var userFilterDto = userTestHelper.convertEntityToFilterDto(one);
            var expectedUserList = List.of(one);
            if (!user.getGroup().getIsInner() && !user.getGroup().equals(one.getGroup())) {
                expectedUserList = Collections.emptyList();
            }
            var response = given().
                    when()
                    .headers(
                            "Authorization",
                            "Bearer " + itHelper.getTokens().get(user.getEmail())
                    )
                    .contentType(APPLICATION_JSON)
                    .body(userFilterDto)
                    .param(SIZE, expectedUserList.size() == 0 ? 1 : expectedUserList.size())
                    .get(USER)
                    .then()
                    .log().body()
                    .statusCode(HttpStatus.OK.value())
                    .extract().response().asString();
            List<User> actualUserList = from(response).getList(CONTENT, User.class);
            assertThat(expectedUserList)
                    .usingElementComparatorIgnoringFields(IGNORE_FIELDS_FOR_COMPARE_USERS)
                    .containsExactlyInAnyOrderElementsOf(actualUserList);
            assertThat(expectedUserList).usingElementComparatorOnFields(ACCOUNT).usingElementComparatorOnFields(ID)
                    .containsExactlyInAnyOrderElementsOf(actualUserList);
            assertThat(expectedUserList).usingElementComparatorOnFields(GROUP).usingElementComparatorOnFields(ID)
                    .containsExactlyInAnyOrderElementsOf(actualUserList);
            assertThat(expectedUserList).usingElementComparatorOnFields(ROLE).usingElementComparatorOnFields(ID)
                    .containsExactlyInAnyOrderElementsOf(actualUserList);
        }
    }

    @Test
    @Order(50)
    void failedGetByFilterByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .get(USER)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());

    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersInnerGroup")
    @Order(60)
    void successGetByIdByAllUsersFromOuterGroups(String userKey, User user) {
        var allUsers = itHelper.getAllUsers();
        for (User one : allUsers) {
            var expectedUserList = List.of(one);
            var response = given().
                    when()
                    .headers(
                            "Authorization",
                            "Bearer " + itHelper.getTokens().get(user.getEmail())
                    )
                    .contentType(APPLICATION_JSON)
                    .param(SIZE, expectedUserList.size())
                    .pathParam(ID, one.getId())
                    .get(USER_BY_ID)
                    .then()
                    .body(ID, equalTo(one.getId().toString()))
                    .log().body()
                    .statusCode(HttpStatus.OK.value());
        }
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersOuterGroup")
    @Order(70)
    void successGetByIdByAllUsersFromInnerGroups(String userKey, User user) {
        var allUsers = itHelper.getAllUsers();
        for (User one : allUsers) {
            if (one.getGroup().equals(user.getGroup())) {
                var expectedUserList = List.of(one);
                var response = given().
                        when()
                        .headers(
                                "Authorization",
                                "Bearer " + itHelper.getTokens().get(user.getEmail())
                        )
                        .contentType(APPLICATION_JSON)
                        .param(SIZE, expectedUserList.size())
                        .pathParam(ID, one.getId())
                        .get(USER_BY_ID)
                        .then()
                        .body(ID, equalTo(one.getId().toString()))
                        .log().body()
                        .statusCode(HttpStatus.OK.value());
            }
        }
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersOuterGroup")
    @Order(80)
    void failedGetByIdByAllUsersFromOuterGroups(String userKey, User user) {
        var allUsers = itHelper.getAllUsers();
        for (User one : allUsers) {
            if (!one.getGroup().equals(user.getGroup())) {
                var response = given().
                        when()
                        .headers(
                                "Authorization",
                                "Bearer " + itHelper.getTokens().get(user.getEmail())
                        )
                        .pathParam(ID, one.getId())
                        .get(USER_BY_ID)
                        .then()
                        .log().body()
                        .statusCode(HttpStatus.FORBIDDEN.value());
            }
        }
    }

    @Test
    @Order(90)
    void failedGetByIdByAnonymousUser() {
        given().
                when()
                .pathParam(ID, UUID.randomUUID())
                .get(USER_BY_ID)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());

    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(100)
    void successCreateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin(String userKey, User user) {
        var allGroups = itHelper.getAllGroup();
        var allRoles = itHelper.getRoleTestHelper().getPredefinedValidEntityList();
        for (Group oneGroup : allGroups) {
            for (Role oneRole : allRoles) {
                if (user.getRole().getWeight() >= oneRole.getWeight()
                        && !oneRole.getName().equals(Roles.ACCOUNT_OWNER.toString())) {
                    var newUser = itHelper.createUserByGivenUserForGivenRoleAndGroupWithoutSaveInMaps(oneGroup, oneRole,
                                                                                                      user
                    );
                    var expectedUserList = List.of(newUser);
                    var response = given().
                            when()
                            .headers(
                                    "Authorization",
                                    "Bearer " + itHelper.getTokens().get(user.getEmail())
                            )
                            .contentType(APPLICATION_JSON)
                            .pathParam(ID, newUser.getId())
                            .get(USER_BY_ID)
                            .then()
                            .log().body()
                            .statusCode(HttpStatus.OK.value())
                            .extract().response().asString();
                    List<User> actualUserList = List.of(from(response).getObject("$", User.class));
                    assertThat(expectedUserList)
                            .usingElementComparatorIgnoringFields(IGNORE_FIELDS_FOR_COMPARE_USERS)
                            .containsExactlyInAnyOrderElementsOf(actualUserList);
                    assertThat(expectedUserList).usingElementComparatorOnFields(ACCOUNT)
                            .usingElementComparatorOnFields(ID)
                            .containsExactlyInAnyOrderElementsOf(actualUserList);
                    assertThat(expectedUserList).usingElementComparatorOnFields(GROUP)
                            .usingElementComparatorOnFields(ID)
                            .containsExactlyInAnyOrderElementsOf(actualUserList);
                    assertThat(expectedUserList).usingElementComparatorOnFields(ROLE).usingElementComparatorOnFields(ID)
                            .containsExactlyInAnyOrderElementsOf(actualUserList);
                }
            }
        }
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFromOuterGroupsWithRoleAdmin")
    @Order(110)
    void successCreateByUsersFromOuterGroupsWithRoleAdmin(String userKey, User user) {
        var allGroups = itHelper.getAllGroup();
        var allRoles = itHelper.getRoleTestHelper().getPredefinedValidEntityList();
        for (Group oneGroup : allGroups) {
            if (user.getGroup().equals(oneGroup)) {
                for (Role oneRole : allRoles) {
                    if (user.getRole().getWeight() >= oneRole.getWeight()) {
                        var newUser =
                                itHelper.createUserByGivenUserForGivenRoleAndGroupWithoutSaveInMaps(oneGroup, oneRole,
                                                                                                    user
                                );
                        var expectedUserList = List.of(newUser);
                        var response = given().
                                when()
                                .headers(
                                        "Authorization",
                                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                                )
                                .contentType(APPLICATION_JSON)
                                .pathParam(ID, newUser.getId())
                                .get(USER_BY_ID)
                                .then()
                                .log().body()
                                .statusCode(HttpStatus.OK.value())
                                .extract().response().asString();
                        List<User> actualUserList = List.of(from(response).getObject("$", User.class));
                        assertThat(expectedUserList)
                                .usingElementComparatorIgnoringFields(IGNORE_FIELDS_FOR_COMPARE_USERS)
                                .containsExactlyInAnyOrderElementsOf(actualUserList);
                        assertThat(expectedUserList).usingElementComparatorOnFields(ACCOUNT)
                                .usingElementComparatorOnFields(ID)
                                .containsExactlyInAnyOrderElementsOf(actualUserList);
                        assertThat(expectedUserList).usingElementComparatorOnFields(GROUP)
                                .usingElementComparatorOnFields(ID)
                                .containsExactlyInAnyOrderElementsOf(actualUserList);
                        assertThat(expectedUserList).usingElementComparatorOnFields(ROLE)
                                .usingElementComparatorOnFields(ID)
                                .containsExactlyInAnyOrderElementsOf(actualUserList);
                    }
                }
            }
        }
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFromOuterGroupsWithRoleAdmin")
    @Order(120)
    void failedCreateByUsersFromOuterGroupsWithRoleAdmin(String userKey, User user) {
        var allGroups = itHelper.getAllGroup();
        var allRoles = itHelper.getRoleTestHelper().getPredefinedValidEntityList();
        for (Group oneGroup : allGroups) {
            if (!user.getGroup().equals(oneGroup)) {
                for (Role oneRole : allRoles) {
                    if (user.getRole().getWeight() >= oneRole.getWeight()) {
                        var newUser = userTestHelper.getRandomValidEntity();
                        var userDtoRequest = userTestHelper.convertUserToUserDtoRequest(newUser, true);
                        userDtoRequest.setRole(oneRole.getId());
                        userDtoRequest.setGroup(oneGroup.getId());
                        given()
                                .headers(
                                        "Authorization",
                                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                                )
                                .contentType(APPLICATION_JSON)
                                .body(userDtoRequest)
                                .post(USER)
                                .then()
                                .log().body()
                                .statusCode(HttpStatus.FORBIDDEN.value());
                    }
                }
            }
        }
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(130)
    void failedCreateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin(String userKey, User user) {
        var allGroups = itHelper.getAllGroup();
        var allRoles = itHelper.getRoleTestHelper().getPredefinedValidEntityList();
        for (Group oneGroup : allGroups) {
            for (Role oneRole : allRoles) {
                if (oneRole.getName().equals(Roles.ACCOUNT_OWNER.toString())) {
                    var newUser = userTestHelper.getRandomValidEntity();
                    var userDtoRequest = userTestHelper.convertUserToUserDtoRequest(newUser, true);
                    userDtoRequest.setRole(oneRole.getId());
                    userDtoRequest.setGroup(oneGroup.getId());
                    given()
                            .headers(
                                    "Authorization",
                                    "Bearer " + itHelper.getTokens().get(user.getEmail())
                            )
                            .contentType(APPLICATION_JSON)
                            .body(userDtoRequest)
                            .post(USER)
                            .then()
                            .log().body()
                            .statusCode(user.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString())
                                                ? HttpStatus.CONFLICT.value()
                                                : HttpStatus.FORBIDDEN.value()
                            );
                }
            }
        }
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFromAnyGroupsWithRolesExecutorAndAuthorAndObserver")
    @Order(140)
    void failedCreateByUsersFromAnyGroupsWithRolesExecutorOrAuthorOrObserver(String userKey, User user) {
        var allGroups = itHelper.getAllGroup();
        var allRoles = itHelper.getRoleTestHelper().getPredefinedValidEntityList();
        for (Group oneGroup : allGroups) {
            for (Role oneRole : allRoles) {
                var newUser = userTestHelper.getRandomValidEntity();
                var userDtoRequest = userTestHelper.convertUserToUserDtoRequest(newUser, true);
                userDtoRequest.setRole(oneRole.getId());
                userDtoRequest.setGroup(oneGroup.getId());
                given()
                        .headers(
                                "Authorization",
                                "Bearer " + itHelper.getTokens().get(user.getEmail())
                        )
                        .contentType(APPLICATION_JSON)
                        .body(userDtoRequest)
                        .post(USER)
                        .then()
                        .log().body()
                        .statusCode(HttpStatus.FORBIDDEN.value());
            }
        }
    }

    @Test
    @Order(150)
    void failedCreateByAnonymousUser() {
        given()
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .post(USER)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @SuppressWarnings("deprecation")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdminAndExecutor")
    @Order(160)
    void successUpdateByUsersFromInnerGroupsWithRolesAccountOwnerOrAdminOrExecutor(String userKey, User user) {
        var allUsers = itHelper.getAllUsers();
        for (User oneUser : allUsers) {
            if (user.getRole().getWeight() >= oneUser.getRole().getWeight()) {
                var nameOfOneUser = oneUser.getName();
                oneUser.setName(nameOfOneUser + "Updated");
                oneUser.generateDisplayName();
                var userDtoRequest = userTestHelper.convertUserToUserDtoRequest(oneUser, false);
                var updatedUser = given()
                        .headers(
                                "Authorization",
                                "Bearer " + itHelper.getTokens().get(user.getEmail())
                        )
                        .contentType(APPLICATION_JSON)
                        .body(userDtoRequest)
                        .put(USER)
                        .then()
                        .log().headers()
                        .log().body()
                        .statusCode(HttpStatus.OK.value())
                        .extract().response().as(User.class);
                oneUser.setVersion(updatedUser.getVersion());
                var expectedUserList = List.of(oneUser);
                var receivedUserById = given().
                        when()
                        .headers(
                                "Authorization",
                                "Bearer " + itHelper.getTokens().get(user.getEmail())
                        )
                        .contentType(APPLICATION_JSON)
                        .pathParam(ID, oneUser.getId())
                        .get(USER_BY_ID)
                        .then()
                        .log().headers()
                        .log().body()
                        .statusCode(HttpStatus.OK.value())
                        .extract().response().as(User.class);
                List<User> actualUserList = List.of(receivedUserById);
                assertThat(expectedUserList)
                        .usingElementComparatorIgnoringFields(IGNORE_FIELDS_FOR_COMPARE_USERS)
                        .containsExactlyInAnyOrderElementsOf(actualUserList);
                assertThat(expectedUserList).usingElementComparatorOnFields(ACCOUNT)
                        .usingElementComparatorOnFields(ID)
                        .containsExactlyInAnyOrderElementsOf(actualUserList);
                assertThat(expectedUserList).usingElementComparatorOnFields(GROUP)
                        .usingElementComparatorOnFields(ID)
                        .containsExactlyInAnyOrderElementsOf(actualUserList);
                assertThat(expectedUserList).usingElementComparatorOnFields(ROLE)
                        .usingElementComparatorOnFields(ID)
                        .containsExactlyInAnyOrderElementsOf(actualUserList);
            }
        }
    }

    @SuppressWarnings("deprecation")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFromOuterGroupsWithRolesAdminAndExecutor")
    @Order(170)
    void successUpdateByUsersFromOuterGroupsWithRolesAdminOrExecutor(String userKey, User user) {
        var allUsers = itHelper.getAllUsers();
        for (User oneUser : allUsers) {
            if (user.getGroup().equals(oneUser.getGroup())
                    && user.getRole().getWeight() >= oneUser.getRole().getWeight()) {
                var nameOfOneUser = oneUser.getName();
                oneUser.setName(nameOfOneUser + "Updated");
                oneUser.generateDisplayName();
                var userDtoRequest = userTestHelper.convertUserToUserDtoRequest(oneUser, false);
                var updatedUser = given()
                        .headers(
                                "Authorization",
                                "Bearer " + itHelper.getTokens().get(user.getEmail())
                        )
                        .contentType(APPLICATION_JSON)
                        .body(userDtoRequest)
                        .put(USER)
                        .then()
                        .log().headers()
                        .log().body()
                        .statusCode(HttpStatus.OK.value())
                        .extract().response().as(User.class);
                oneUser.setVersion(updatedUser.getVersion());
                var expectedUserList = List.of(oneUser);
                var receivedUserById = given().
                        when()
                        .headers(
                                "Authorization",
                                "Bearer " + itHelper.getTokens().get(user.getEmail())
                        )
                        .contentType(APPLICATION_JSON)
                        .pathParam(ID, oneUser.getId())
                        .get(USER_BY_ID)
                        .then()
                        .log().headers()
                        .log().body()
                        .statusCode(HttpStatus.OK.value())
                        .extract().response().as(User.class);
                List<User> actualUserList = List.of(receivedUserById);
                assertThat(expectedUserList)
                        .usingElementComparatorIgnoringFields(IGNORE_FIELDS_FOR_COMPARE_USERS)
                        .containsExactlyInAnyOrderElementsOf(actualUserList);
                assertThat(expectedUserList).usingElementComparatorOnFields(ACCOUNT)
                        .usingElementComparatorOnFields(ID)
                        .containsExactlyInAnyOrderElementsOf(actualUserList);
                assertThat(expectedUserList).usingElementComparatorOnFields(GROUP)
                        .usingElementComparatorOnFields(ID)
                        .containsExactlyInAnyOrderElementsOf(actualUserList);
                assertThat(expectedUserList).usingElementComparatorOnFields(ROLE)
                        .usingElementComparatorOnFields(ID)
                        .containsExactlyInAnyOrderElementsOf(actualUserList);
            }
        }
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFromOuterGroupsWithRolesAdminAndExecutor")
    @Order(180)
    void failedUpdateByUsersFromOuterGroupsWithRolesAdminOrExecutor(String userKey, User user) {
        var allUsers = itHelper.getAllUsers();
        for (User oneUser : allUsers) {
            if (!user.getGroup().equals(oneUser.getGroup())
                    && user.getRole().getWeight() >= oneUser.getRole().getWeight()) {
                var userDtoRequest = userTestHelper.convertUserToUserDtoRequest(oneUser, false);
                given()
                        .headers(
                                "Authorization",
                                "Bearer " + itHelper.getTokens().get(user.getEmail())
                        )
                        .contentType(APPLICATION_JSON)
                        .body(userDtoRequest)
                        .put(USER)
                        .then()
                        .log().headers()
                        .log().body()
                        .statusCode(HttpStatus.FORBIDDEN.value());
            }
        }
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdminAndExecutor")
    @Order(190)
    void failedUpdateByUsersFromInnerGroupsWithRolesAccountOwnerOrAdminOrExecutor(String userKey, User user) {
        var allUsers = itHelper.getAllUsers();
        for (User oneUser : allUsers) {
            if (user.getRole().getWeight() < oneUser.getRole().getWeight()) {
                var userDtoRequest = userTestHelper.convertUserToUserDtoRequest(oneUser, false);
                given()
                        .headers(
                                "Authorization",
                                "Bearer " + itHelper.getTokens().get(user.getEmail())
                        )
                        .contentType(APPLICATION_JSON)
                        .body(userDtoRequest)
                        .put(USER)
                        .then()
                        .log().headers()
                        .log().body()
                        .statusCode(user.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString())
                                            ? HttpStatus.CONFLICT.value()
                                            : HttpStatus.FORBIDDEN.value());
            }
        }
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFromOuterGroupsWithRolesAdminAndExecutor")
    @Order(200)
    void failedUpdateByUsersFromOuterGroupsWithRolesAdminOrExecutorForReasonOfWeight(String userKey, User user) {
        var allUsers = itHelper.getAllUsers();
        for (User oneUser : allUsers) {
            if (user.getGroup().equals(oneUser.getGroup())
                    && user.getRole().getWeight() < oneUser.getRole().getWeight()) {
                var userDtoRequest = userTestHelper.convertUserToUserDtoRequest(oneUser, false);
                given()
                        .headers(
                                "Authorization",
                                "Bearer " + itHelper.getTokens().get(user.getEmail())
                        )
                        .contentType(APPLICATION_JSON)
                        .body(userDtoRequest)
                        .put(USER)
                        .then()
                        .log().headers()
                        .log().body()
                        .statusCode(HttpStatus.FORBIDDEN.value());
            }
        }
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersFormAnyGroupWithRolesAuthorAndObserver")
    @Order(210)
    void failedUpdateByUsersFromAnyGroupsWithRolesAuthorOrObserver(String userKey, User user) {
        var allUsers = itHelper.getAllUsers();
        for (User oneUser : allUsers) {
                var userDtoRequest = userTestHelper.convertUserToUserDtoRequest(oneUser, false);
                given()
                        .headers(
                                "Authorization",
                                "Bearer " + itHelper.getTokens().get(user.getEmail())
                        )
                        .contentType(APPLICATION_JSON)
                        .body(userDtoRequest)
                        .put(USER)
                        .then()
                        .log().headers()
                        .log().body()
                        .statusCode(HttpStatus.FORBIDDEN.value());
        }
    }

    @Test
    @Order(220)
    void failedUpdateByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .put(USER)
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
