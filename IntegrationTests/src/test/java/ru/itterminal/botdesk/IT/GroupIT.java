package ru.itterminal.botdesk.IT;

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
import ru.itterminal.botdesk.IT.util.ITHelper;
import ru.itterminal.botdesk.IT.util.ITTestConfig;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.test.GroupTestHelper;
import ru.itterminal.botdesk.aau.repository.UserRepository;
import ru.itterminal.botdesk.security.jwt.JwtProvider;

import java.util.List;
import java.util.UUID;
import java.util.stream.Stream;

import static io.restassured.RestAssured.given;
import static io.restassured.path.json.JsonPath.from;
import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static ru.itterminal.botdesk.IT.util.ITHelper.*;

@DataJpaTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class, JwtProvider.class})
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@TestPropertySource(properties = {"jwt.token.secret=ksedtob", "jwt.token.expired=8640000", "jwt.token.prefix=Bearer"})
class GroupIT {


    @Autowired
    private UserRepository userRepository;

    private static ITHelper itHelper;

    private final GroupTestHelper groupTestHelper = new GroupTestHelper();


    @BeforeAll
    void beforeAll() {
        RestAssured.useRelaxedHTTPSValidation();
        RestAssured.baseURI = "https://localhost:8080";
        RestAssured.basePath = "/api/v1/";
        itHelper = new ITHelper();
        itHelper.createAndVerifyAccount(userRepository);
        itHelper.createInitialInnerAndOuterGroups(1, 2);
        var allRolesWithoutAccountOwner = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(ITHelper.ADMIN,
                        ITHelper.AUTHOR,
                        ITHelper.EXECUTOR,
                        ITHelper.OBSERVER
                )
        );
        itHelper.createUsersForEachRoleInGroup(itHelper.getOuterGroup().get(OUTER_GROUP + 1), allRolesWithoutAccountOwner);
        itHelper.createUsersForEachRoleInGroup(itHelper.getInnerGroup().get(INNER_GROUP + 1), allRolesWithoutAccountOwner);
        itHelper.createUsersForEachRoleInGroup(itHelper.getOuterGroup().get(OUTER_GROUP + 2), allRolesWithoutAccountOwner);
        itHelper.createUsersForEachRoleInGroup(itHelper.getInnerGroup().get(INNER_GROUP + 2), allRolesWithoutAccountOwner);
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersInnerGroup")
    @Order(10)
    void successGetByFilterAllGroupsByAllUsersInnerGroup(String userKey, User user) {
        var expectedGroupList = itHelper.getAllGroup();
        int expectedCountGroup = expectedGroupList.size();
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail()))
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .param("size", expectedCountGroup)
                .get(GROUP)
                .then()
                .log().body()
                .body("totalElements", equalTo(expectedCountGroup))
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        List<Group> actualGroupList = from(response).getList("content", Group.class);
        assertThat(expectedGroupList).usingElementComparatorIgnoringFields("account").containsExactlyInAnyOrderElementsOf(actualGroupList);
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersOuterGroup")
    @Order(20)
    void successGetByFilterAllOuterGroupByAllUsersOuterGroup(String userKey, User user) {
        var expectedGroup = List.of(user.getGroup());
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail()))
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .get(GROUP)
                .then()
                .log().body()
                .body("totalElements", equalTo(1))
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        var actualGroup = from(response).getList("content", Group.class);
        assertThat(expectedGroup).usingElementComparatorIgnoringFields("account").containsExactlyInAnyOrderElementsOf(actualGroup);
    }

    @Test
    @Order(30)
    void failedGetByFilterByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .get(GROUP)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());

    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersInnerGroup")
    @Order(40)
    void successGetByIdByAllUsersInnerGroup(String userKey, User user) {
        var groupList = itHelper.getAllGroup();
        for (Group group : groupList) {
            given().
                    when()
                    .headers(
                            "Authorization",
                            "Bearer " + itHelper.getTokens().get(user.getEmail()))
                    .pathParam(ID, group.getId())
                    .get(GROUP_BY_ID)
                    .then()
                    .body(ID, equalTo(group.getId().toString()))
                    .log().body()
                    .statusCode(HttpStatus.OK.value());
        }
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersOuterGroup")
    @Order(50)
    void successGetByIdOwnGroupByAllUsersOuterGroup(String userKey, User user) {
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail()))
                .pathParam(ID, user.getGroup().getId())
                .get(GROUP_BY_ID)
                .then()
                .body(ID, equalTo(user.getGroup().getId().toString()))
                .log().body()
                .statusCode(HttpStatus.OK.value());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersOuterGroup")
    @Order(60)
    void failedGetByIdNotOwnGroupByAllUsersOuterGroup(String userKey, User user) {
        var allGroupList = itHelper.getAllGroup();
        for (Group group : allGroupList) {
            if (!group.equals(user.getGroup())) {
                given().
                        when()
                        .headers(
                                "Authorization",
                                "Bearer " + itHelper.getTokens().get(user.getEmail()))
                        .pathParam(ID, group.getId())
                        .get(GROUP_BY_ID)
                        .then()
                        .log().body()
                        .statusCode(HttpStatus.FORBIDDEN.value());
            }
        }
    }

    @Test
    @Order(70)
    void failedGetByIdByAnonymousUser() {
        given().
                when()
                .pathParam(ID, UUID.randomUUID())
                .get(GROUP_BY_ID)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());

    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(80)
    void successGetByFilterGroupByAllUsersByFilterFromOwnGroup(String userKey, User user) {
        var expectedGroup = List.of(user.getGroup());
        var filterDto = groupTestHelper.convertEntityToFilterDto(user.getGroup());
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail()))
                .contentType(APPLICATION_JSON)
                .body(filterDto)
                .get(GROUP)
                .then()
                .log().body()
                .body("totalElements", equalTo(1))
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        var actualGroup = from(response).getList("content", Group.class);
        assertThat(expectedGroup).usingElementComparatorIgnoringFields("account").containsExactlyInAnyOrderElementsOf(actualGroup);
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(90)
    void successGetByFilter_ByAllUsersAndAllGroups(String userKey, User user) {
        var allGroupList = itHelper.getAllGroup();
        for (Group group : allGroupList) {
            var filterDto = groupTestHelper.convertEntityToFilterDto(group);
            var response = given().
                    when()
                    .headers(
                            "Authorization",
                            "Bearer " + itHelper.getTokens().get(user.getEmail()))
                    .contentType(APPLICATION_JSON)
                    .body(filterDto)
                    .get(GROUP)
                    .then()
                    .log().body()
                    .statusCode(HttpStatus.OK.value())
                    .extract().response().asString();
            var actualGroup = from(response).getList("content", Group.class);
            if (user.getGroup().getIsInner() || user.getGroup().equals(group)) {
                assertThat(List.of(group)).usingElementComparatorIgnoringFields("account").containsExactlyInAnyOrderElementsOf(actualGroup);
            } else {
                assertEquals(0, actualGroup.size());
            }
        }
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(100)
    void successCreateAnyGroupByUsersInnerGroupWithRolesAccountOwnerOrAdmin(String userKey, User user) {
        var newGroup = groupTestHelper.getRandomValidEntity();
        var groupDto = groupTestHelper.convertEntityToDtoRequest(newGroup, true);
        var actualGroup = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail()))
                .contentType(APPLICATION_JSON)
                .body(groupDto)
                .post(GROUP)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(Group.class);
        assertThat(List.of(newGroup)).usingElementComparatorOnFields("name", "outId", "comment", "isInner")
                .containsExactlyInAnyOrderElementsOf(List.of(actualGroup));
        assertFalse(actualGroup.getIsDeprecated());
        assertFalse(actualGroup.getDeleted());
        itHelper.putGroup(actualGroup);
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdmin")
    @Order(110)
    void failedCreateGroupByUsersInnerGroupWithRolesAccountOwnerOrdAdmin_nameIsNotUnique(String userKey, User user) {
        var newGroup = itHelper.getInnerGroup().get(INNER_GROUP_1);
        var groupDto = groupTestHelper.convertEntityToDtoRequest(newGroup, true);
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail()))
                .contentType(APPLICATION_JSON)
                .body(groupDto)
                .post(GROUP)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value());
    }

    @Test
    @Order(120)
    void successCreateGroupByUsersWithRoleAccountOwner_nameOfGroupNotUniqueForInnerAndOuterGroup() {
        var newGroup = itHelper.getInnerGroup().get(INNER_GROUP_1).toBuilder().build();
        newGroup.setIsInner(false);
        var groupDto = groupTestHelper.convertEntityToDtoRequest(newGroup, true);
        var actualGroup = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail()))
                .contentType(APPLICATION_JSON)
                .body(groupDto)
                .post(GROUP)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(Group.class);
        assertThat(List.of(newGroup)).usingElementComparatorOnFields("name", "outId", "comment", "isInner")
                .containsExactlyInAnyOrderElementsOf(List.of(actualGroup));
        assertFalse(actualGroup.getIsDeprecated());
        assertFalse(actualGroup.getDeleted());
        itHelper.putGroup(actualGroup);
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersOuterGroup")
    @Order(130)
    void failedCreateGroupByAllUsersOuterGroup(String userKey, User user) {
        var newGroup = groupTestHelper.getRandomValidEntity();
        var groupDto = groupTestHelper.convertEntityToDtoRequest(newGroup, true);
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail()))
                .contentType(APPLICATION_JSON)
                .body(groupDto)
                .post(GROUP)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesExecutorAndAuthorAndObserver")
    @Order(140)
    void failedCreateGroupByUsersInnerGroupWithRolesExecutorOrAuthorOrObserver(String userKey, User user) {
        var newGroup = groupTestHelper.getRandomValidEntity();
        var groupDto = groupTestHelper.convertEntityToDtoRequest(newGroup, true);
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail()))
                .contentType(APPLICATION_JSON)
                .body(groupDto)
                .post(GROUP)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdminAndExecutor")
    @Order(150)
    void successUpdateGroupByUsersInnerGroupWithRolesAccountOwnerOrAdminOrExecutor(String userKey, User user) {
        var allGroupList = itHelper.getAllGroup();
        for (Group expectedGroup : allGroupList) {
            var groupDto = groupTestHelper.convertEntityToDtoRequest(expectedGroup, false);
            var actualGroup = given().
                    when()
                    .headers(
                            "Authorization",
                            "Bearer " + itHelper.getTokens().get(user.getEmail()))
                    .contentType(APPLICATION_JSON)
                    .body(groupDto)
                    .put(GROUP)
                    .then()
                    .log().body()
                    .statusCode(HttpStatus.OK.value())
                    .extract().response().as(Group.class);
            assertThat(List.of(expectedGroup)).usingElementComparatorOnFields("name", "outId", "comment", "isInner", "isDeprecated", "deleted")
                    .containsExactlyInAnyOrderElementsOf(List.of(actualGroup));
        }
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdminAndExecutor")
    @Order(160)
    void failedUpdateGroupByUsersInnerGroupWithRolesAccountOwnerOrAdminOrExecutor_nameIsNotUnique(String userKey, User user) {
        var groupForUpdate = itHelper.getInnerGroup().get(INNER_GROUP_1).toBuilder().build();
        var nameOfInnerGroup2 = itHelper.getInnerGroup().get(INNER_GROUP + 2).getName();
        var groupDto = groupTestHelper.convertEntityToDtoRequest(groupForUpdate, false);
        groupDto.setName(nameOfInnerGroup2);
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail()))
                .contentType(APPLICATION_JSON)
                .body(groupDto)
                .put(GROUP)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value());

    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAccountOwnerAndAdminAndExecutor")
    @Order(170)
    void failedUpdateGroupByUsersInnerGroupWithRolesAccountOwnerOrAdminOrExecutor_changeIsInner(String userKey, User user) {
        var allGroupList = itHelper.getAllGroup();
        for (Group group : allGroupList) {
            var groupDto = groupTestHelper.convertEntityToDtoRequest(group, false);
            groupDto.setIsInner(!group.getIsInner());
            given().
                    when()
                    .headers(
                            "Authorization",
                            "Bearer " + itHelper.getTokens().get(user.getEmail()))
                    .contentType(APPLICATION_JSON)
                    .body(groupDto)
                    .put(GROUP)
                    .then()
                    .log().body()
                    .statusCode(HttpStatus.BAD_REQUEST.value());
        }
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamUsersInnerGroupWithRolesAuthorAndObserver")
    @Order(180)
    void failedUpdateGroupByAllUsersInnerGroupWithRolesAuthorOrObserver(String userKey, User user) {
        var allGroupList = itHelper.getAllGroup();
        for (Group group : allGroupList) {
            var groupDto = groupTestHelper.convertEntityToDtoRequest(group, false);
            given().
                    when()
                    .headers(
                            "Authorization",
                            "Bearer " + itHelper.getTokens().get(user.getEmail()))
                    .contentType(APPLICATION_JSON)
                    .body(groupDto)
                    .put(GROUP)
                    .then()
                    .log().body()
                    .statusCode(HttpStatus.FORBIDDEN.value());
        }
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersOuterGroup")
    @Order(190)
    void failedUpdateGroupByAllUsersOuterGroup(String userKey, User user) {
        var allGroupList = itHelper.getAllGroup();
        for (Group group : allGroupList) {
            var groupDto = groupTestHelper.convertEntityToDtoRequest(group, false);
            given().
                    when()
                    .headers(
                            "Authorization",
                            "Bearer " + itHelper.getTokens().get(user.getEmail()))
                    .contentType(APPLICATION_JSON)
                    .body(groupDto)
                    .put(GROUP)
                    .then()
                    .log().body()
                    .statusCode(HttpStatus.FORBIDDEN.value());
        }
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
                List.of(ACCOUNT_OWNER,
                        ADMIN
                )
        );
        return itHelper.getStreamUsers(roles, true);
    }

    private static Stream<Arguments> getStreamUsersInnerGroupWithRolesExecutorAndAuthorAndObserver() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(EXECUTOR,
                        AUTHOR,
                        OBSERVER
                )
        );
        return itHelper.getStreamUsers(roles, true);
    }

    private static Stream<Arguments> getStreamUsersInnerGroupWithRolesAccountOwnerAndAdminAndExecutor() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(ACCOUNT_OWNER,
                        ADMIN,
                        EXECUTOR
                )
        );
        return itHelper.getStreamUsers(roles, true);
    }

    private static Stream<Arguments> getStreamUsersInnerGroupWithRolesAuthorAndObserver() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(AUTHOR,
                        OBSERVER
                )
        );
        return itHelper.getStreamUsers(roles, true);
    }
}
