package ru.itterminal.yanmas.IT.AAU;

import static io.restassured.RestAssured.given;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static ru.itterminal.yanmas.IT.util.ITHelper.ACCOUNT_OWNER;
import static ru.itterminal.yanmas.IT.util.ITHelper.ADMIN;
import static ru.itterminal.yanmas.IT.util.ITHelper.APPLICATION_JSON;
import static ru.itterminal.yanmas.IT.util.ITHelper.AUTHOR;
import static ru.itterminal.yanmas.IT.util.ITHelper.EMPTY_BODY;
import static ru.itterminal.yanmas.IT.util.ITHelper.EXECUTOR;
import static ru.itterminal.yanmas.IT.util.ITHelper.OBSERVER;
import static ru.itterminal.yanmas.IT.util.ITHelper.WATCHED_ENTITIES;

import java.util.ArrayList;
import java.util.Arrays;
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

import io.restassured.RestAssured;
import ru.itterminal.yanmas.IT.util.ITHelper;
import ru.itterminal.yanmas.IT.util.ITTestConfig;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.model.WhoWatchedEntity;
import ru.itterminal.yanmas.aau.model.dto.WhoWatchedEntityDtoRequest;
import ru.itterminal.yanmas.aau.repository.UserRepository;
import ru.itterminal.yanmas.aau.repository.WhoWatchedEntityRepository;

@SuppressWarnings("unused")
@DataJpaTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class})
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class WhoWatchedEntityIT {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private WhoWatchedEntityRepository whoWatchedEntityRepository;

    private static final ITHelper itHelper = new ITHelper();
    private final List<UUID> entitiesId = Arrays.asList(
            UUID.randomUUID(),
            UUID.randomUUID(),
            UUID.randomUUID(),
            UUID.randomUUID(),
            UUID.randomUUID()
    );

    @BeforeAll
    void beforeAll() {
        RestAssured.useRelaxedHTTPSValidation();
        RestAssured.baseURI = "https://localhost:8080";
        RestAssured.basePath = "/api/v1/";
        itHelper.createAndVerifyAccount(userRepository);
        itHelper.createInitialInnerAndOuterGroups(1, 2);
        itHelper.createInitialUsers();
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(10)
    void successInitialWatchedEntitiesByAllUsers(String userKey, User user) {
        var request = WhoWatchedEntityDtoRequest.builder()
                .entitiesId(entitiesId)
                .build();
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(request)
                .post(WATCHED_ENTITIES)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value());
        var accountId = itHelper.getAccount().getId();
        var userId = user.getId();
        var watchedEntitiesIdList =
                whoWatchedEntityRepository.findAllByAccountIdAndEntityIdInAndUserId(accountId, entitiesId, userId);
        var actualEntitiesId =
                watchedEntitiesIdList.stream().map(WhoWatchedEntity::getEntityId).collect(Collectors.toList());
        assertThat(entitiesId).containsAll(actualEntitiesId);
        assertEquals(entitiesId.size(), actualEntitiesId.size());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(20)
    void successRepeatWatchedEntitiesByAllUsers(String userKey, User user) {
        var request = WhoWatchedEntityDtoRequest.builder()
                .entitiesId(entitiesId)
                .build();
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(request)
                .post(WATCHED_ENTITIES)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value());
        var accountId = itHelper.getAccount().getId();
        var userId = user.getId();
        var watchedEntitiesIdList =
                whoWatchedEntityRepository.findAllByAccountIdAndEntityIdInAndUserId(accountId, entitiesId, userId);
        var actualEntitiesId =
                watchedEntitiesIdList.stream().map(WhoWatchedEntity::getEntityId).collect(Collectors.toList());
        assertThat(entitiesId).containsAll(actualEntitiesId);
        assertEquals(entitiesId.size(), actualEntitiesId.size());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(30)
    void successRepeatAndInitialWatchedEntitiesByAllUsers(String userKey, User user) {
        var entitiesIdNew = new ArrayList<>(entitiesId);
        entitiesIdNew.add(UUID.randomUUID());
        var request = WhoWatchedEntityDtoRequest.builder()
                .entitiesId(entitiesIdNew)
                .build();
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(request)
                .post(WATCHED_ENTITIES)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value());
        var accountId = itHelper.getAccount().getId();
        var userId = user.getId();
        var watchedEntitiesIdList = whoWatchedEntityRepository.findAllByAccountIdAndEntityIdInAndUserId(
                accountId,
                entitiesIdNew,
                userId
        );
        var actualEntitiesId =
                watchedEntitiesIdList.stream().map(WhoWatchedEntity::getEntityId).collect(Collectors.toList());
        assertThat(entitiesIdNew).containsAll(actualEntitiesId);
        assertEquals(entitiesIdNew.size(), actualEntitiesId.size());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(40)
    void failedWatchedEntitiesByAllUsers_whenEntitiesIdListIsNull(String userKey, User user) {
        var request = WhoWatchedEntityDtoRequest.builder()
                .entitiesId(null)
                .build();
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(request)
                .post(WATCHED_ENTITIES)
                .then()
                .log().body()
                .statusCode(HttpStatus.BAD_REQUEST.value());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(50)
    void failedWatchedEntitiesByAllUsers_whenEntitiesIdListIsEmpty(String userKey, User user) {
        var request = WhoWatchedEntityDtoRequest.builder()
                .entitiesId(Collections.emptyList())
                .build();
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(request)
                .post(WATCHED_ENTITIES)
                .then()
                .log().body()
                .statusCode(HttpStatus.BAD_REQUEST.value());
    }

    @Test
    @Order(60)
    void failedWatchedEntitiesByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .delete(WATCHED_ENTITIES)
                .then()
                .log().body()
                .statusCode(HttpStatus.UNAUTHORIZED.value());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(70)
    void successInitialUnwatchedEntitiesByAllUsers(String userKey, User user) {
        var entitiesIdNew = new ArrayList<>(entitiesId);
        entitiesIdNew.remove(entitiesId.size() - 1);
        var request = WhoWatchedEntityDtoRequest.builder()
                .entitiesId(entitiesIdNew)
                .build();
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(request)
                .delete(WATCHED_ENTITIES)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value());
        var accountId = itHelper.getAccount().getId();
        var userId = user.getId();
        var watchedEntitiesIdList =
                whoWatchedEntityRepository.findAllByAccountIdAndEntityIdInAndUserId(accountId, entitiesId, userId);
        var actualEntitiesId =
                watchedEntitiesIdList.stream().map(WhoWatchedEntity::getEntityId).collect(Collectors.toList());
        assertThat(actualEntitiesId).containsExactly(entitiesId.get(entitiesId.size() - 1));
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(80)
    void successRepeatUnwatchedEntitiesByAllUsers(String userKey, User user) {
        var entitiesIdNew = new ArrayList<>(entitiesId);
        entitiesIdNew.remove(entitiesId.size() - 1);
        var request = WhoWatchedEntityDtoRequest.builder()
                .entitiesId(entitiesIdNew)
                .build();
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(request)
                .delete(WATCHED_ENTITIES)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value());
        var accountId = itHelper.getAccount().getId();
        var userId = user.getId();
        var watchedEntitiesIdList =
                whoWatchedEntityRepository.findAllByAccountIdAndEntityIdInAndUserId(accountId, entitiesId, userId);
        var actualEntitiesId =
                watchedEntitiesIdList.stream().map(WhoWatchedEntity::getEntityId).collect(Collectors.toList());
        assertThat(actualEntitiesId).containsExactly(entitiesId.get(entitiesId.size() - 1));
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(90)
    void successRepeatAndInitialUnwatchedEntitiesByAllUsers(String userKey, User user) {
        var request = WhoWatchedEntityDtoRequest.builder()
                .entitiesId(entitiesId)
                .build();
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(request)
                .delete(WATCHED_ENTITIES)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value());
        var accountId = itHelper.getAccount().getId();
        var userId = user.getId();
        var watchedEntitiesIdList =
                whoWatchedEntityRepository.findAllByAccountIdAndEntityIdInAndUserId(accountId, entitiesId, userId);
        assertEquals(0, watchedEntitiesIdList.size());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(100)
    void failedUnwatchedEntitiesByAllUsers_whenEntitiesIdListIsNull(String userKey, User user) {
        var request = WhoWatchedEntityDtoRequest.builder()
                .entitiesId(null)
                .build();
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(request)
                .delete(WATCHED_ENTITIES)
                .then()
                .log().body()
                .statusCode(HttpStatus.BAD_REQUEST.value());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsers")
    @Order(110)
    void failedUnwatchedEntitiesByAllUsers_whenEntitiesIdListIsEmpty(String userKey, User user) {
        var request = WhoWatchedEntityDtoRequest.builder()
                .entitiesId(Collections.emptyList())
                .build();
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(request)
                .delete(WATCHED_ENTITIES)
                .then()
                .log().body()
                .statusCode(HttpStatus.BAD_REQUEST.value());
    }

    @Test
    @Order(120)
    void failedUnwatchedEntitiesByAnonymousUser() {
        given().
                when()
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .delete(WATCHED_ENTITIES)
                .then()
                .log().body()
                .statusCode(HttpStatus.UNAUTHORIZED.value());
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
