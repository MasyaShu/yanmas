package ru.itterminal.yanmas.IT.AAU;

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
import ru.itterminal.yanmas.IT.util.ITHelper;
import ru.itterminal.yanmas.IT.util.ITTestConfig;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.model.WhoWatchedEntity;
import ru.itterminal.yanmas.aau.model.dto.WhoWatchedEntityDtoRequest;
import ru.itterminal.yanmas.aau.repository.UserRepository;
import ru.itterminal.yanmas.aau.repository.WhoWatchedEntityRepository;
import ru.itterminal.yanmas.aau.service.impl.WhoWatchedEntityServiceImpl;
import ru.itterminal.yanmas.commons.exception.error.ApiError;
import ru.itterminal.yanmas.security.jwt.JwtProvider;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;
import ru.itterminal.yanmas.tickets.model.SettingsAccessToTicketTypes;
import ru.itterminal.yanmas.tickets.model.test.SettingsAccessToTicketTypesTestHelper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static io.restassured.RestAssured.given;
import static io.restassured.path.json.JsonPath.from;
import static java.lang.String.format;
import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static ru.itterminal.yanmas.IT.util.ITHelper.*;
import static ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl.NOT_UNIQUE_CODE;
import static ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl.NOT_UNIQUE_MESSAGE;
import static ru.itterminal.yanmas.tickets.service.validator.SettingsAccessToTicketTypesOperationValidator.THIS_KEY_OF_SETTINGS_ACCOUNT_ID_GROUP_ID_USER_ID;

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
    void successWatchedEntitiesByAllUsers(String userKey, User user) {
        var request = WhoWatchedEntityDtoRequest.builder()
                .entitiesId(entitiesId)
                .build();
        var response = given().
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
        var watchedEntitiesIdList = whoWatchedEntityRepository.findAllByAccountIdAndEntityIdInAndUserId(accountId, entitiesId, userId);
        var actualEntitiesId = watchedEntitiesIdList.stream().map(WhoWatchedEntity::getEntityId).collect(Collectors.toList());
        assertThat(entitiesId).containsAll(actualEntitiesId);
        assertEquals(entitiesId.size(), actualEntitiesId.size());
    }

//    @ParameterizedTest(name = "{index} User: {0}")
//    @MethodSource("getStreamAllUsers")
//    @Order(20)
//    void failedGetByFilter_whenUserIsNotInnerGroupAndNotHaveRoleAccountOwnerOrAdmin(String userKey,
//                                                                                    User user) {
//        if (user.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString()) ||
//                (user.getRole().getName().equals(Roles.ADMIN.toString()) && user.getGroup().getIsInner())) {
//            return;
//        }
//        var expectedEntity =
//                itHelper.getSettingsAccessToTicketTypes().values().stream().collect(Collectors.toList());
//        given().
//                when()
//                .headers(
//                        "Authorization",
//                        "Bearer " + itHelper.getTokens().get(user.getEmail())
//                )
//                .contentType(APPLICATION_JSON)
//                .body(EMPTY_BODY)
//                .param(SIZE, expectedEntity.size())
//                .get(SETTING_ACCESS_TO_TICKET_VIA_TICKET_TYPES)
//                .then()
//                .log().body()
//                .statusCode(HttpStatus.FORBIDDEN.value());
//    }
//
//    @Test
//    @Order(30)
//    void failedGetByFilterByAnonymousUser() {
//        given().
//                when()
//                .contentType(APPLICATION_JSON)
//                .body(EMPTY_BODY)
//                .get(SETTING_ACCESS_TO_TICKET_VIA_TICKET_TYPES)
//                .then()
//                .log().body()
//                .statusCode(HttpStatus.UNAUTHORIZED.value());
//    }
//
//    @ParameterizedTest(name = "{index} User: {0}")
//    @MethodSource("getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin")
//    @Order(40)
//    void successGetByIdByAllUsersFromInnerGroupWithRolesAccountOwnerAndAdmin(String userKey, User user) {
//        var expectedEntity =
//                itHelper.getSettingsAccessToTicketTypes()
//                        .get(INITIAL_SETTINGS_ACCESS_TO_TICKET_TYPES);
//        var actualEntity = given().
//                when().
//                headers(
//                        "Authorization",
//                        "Bearer " + itHelper.getTokens().get(user.getEmail())
//                )
//                .contentType(APPLICATION_JSON)
//                .pathParam(ID, expectedEntity.getId())
//                .get(SETTINGS_ACCESS_TO_TICKET_VIA_TICKET_TYPES_BY_ID)
//                .then()
//                .log().body()
//                .body(ID, equalTo(expectedEntity.getId().toString()))
//                .statusCode(HttpStatus.OK.value())
//                .extract().response().as(SettingsAccessToTicketTypes.class);
//        assertThat(actualEntity).usingRecursiveComparison().ignoringActualNullFields()
//                .isEqualTo(expectedEntity);
//    }
//
//    @ParameterizedTest(name = "{index} User: {0}")
//    @MethodSource("getStreamAllUsers")
//    @Order(50)
//    void failedGetById_whenUserIsNotInnerGroupAndNotHaveRoleAccountOwnerOrAdmin(String userKey,
//                                                                                User user) {
//        if (user.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString()) ||
//                (user.getRole().getName().equals(Roles.ADMIN.toString()) && user.getGroup().getIsInner())) {
//            return;
//        }
//        var expectedEntity =
//                itHelper.getSettingsAccessToTicketTypes()
//                        .get(INITIAL_SETTINGS_ACCESS_TO_TICKET_TYPES);
//        given().
//                when().
//                headers(
//                        "Authorization",
//                        "Bearer " + itHelper.getTokens().get(user.getEmail())
//                )
//                .contentType(APPLICATION_JSON)
//                .pathParam(ID, expectedEntity.getId())
//                .get(SETTINGS_ACCESS_TO_TICKET_VIA_TICKET_TYPES_BY_ID)
//                .then()
//                .log().body()
//                .statusCode(HttpStatus.FORBIDDEN.value());
//    }
//
//    @Test
//    @Order(60)
//    void failedGetByIdByAnonymousUser() {
//        given().
//                when()
//                .contentType(APPLICATION_JSON)
//                .pathParam(ID, UUID.randomUUID().toString())
//                .get(SETTINGS_ACCESS_TO_TICKET_VIA_TICKET_TYPES_BY_ID)
//                .then()
//                .log().body()
//                .statusCode(HttpStatus.UNAUTHORIZED.value());
//    }
//
//    @ParameterizedTest(name = "{index} User: {0}")
//    @MethodSource("getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin")
//    @Order(70)
//    void successCreateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin(String userKey, User user) {
//        var expectedEntity = itHelper.getSettingsAccessToTicketTypes().get(INITIAL_SETTINGS_ACCESS_TO_TICKET_TYPES).toBuilder()
//                .user(user)
//                .deleted(null)
//                .displayName(null)
//                .id(null)
//                .version(null)
//                .build();
//        var request = settingsTestHelper.convertEntityToDtoRequest(expectedEntity, true);
//        var actualEntity = given().
//                when().
//                headers(
//                        "Authorization",
//                        "Bearer " + itHelper.getTokens().get(user.getEmail())
//                )
//                .contentType(APPLICATION_JSON)
//                .body(request)
//                .post(SETTING_ACCESS_TO_TICKET_VIA_TICKET_TYPES)
//                .then()
//                .log().body()
//                .statusCode(HttpStatus.CREATED.value())
//                .extract().response().as(SettingsAccessToTicketTypes.class);
//        assertEquals(expectedEntity.getUser().getId(), actualEntity.getUser().getId());
//    }
//
//    @ParameterizedTest(name = "{index} User: {0}")
//    @MethodSource("getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin")
//    @Order(80)
//    void failedCreateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin_whenKeysIsNotUnique(String userKey,
//                                                                                             User user) {
//        var createdEntity = itHelper.getSettingsAccessToTicketTypes().get(INITIAL_SETTINGS_ACCESS_TO_TICKET_TYPES).toBuilder()
//                .build();
//        var request = settingsTestHelper.convertEntityToDtoRequest(createdEntity, true);
//        var apiError = given().
//                when().
//                headers(
//                        "Authorization",
//                        "Bearer " + itHelper.getTokens().get(user.getEmail())
//                )
//                .contentType(APPLICATION_JSON)
//                .body(request)
//                .post(SETTING_ACCESS_TO_TICKET_VIA_TICKET_TYPES)
//                .then()
//                .log().body()
//                .body(STATUS, equalTo(HttpStatus.CONFLICT.value()))
//                .extract().response().as(ApiError.class);
//        assertEquals(
//                format(NOT_UNIQUE_MESSAGE, THIS_KEY_OF_SETTINGS_ACCOUNT_ID_GROUP_ID_USER_ID),
//                apiError.getErrors().get(NOT_UNIQUE_CODE).get(0).getMessage()
//        );
//    }
//
//    @ParameterizedTest(name = "{index} User: {0}")
//    @MethodSource("getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin")
//    @Order(90)
//    void failedCreateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin_whenGroupOfTicketTypesIsNull
//            (String userKey, User user) {
//        var createdEntity = itHelper.getSettingsAccessToTicketTypes().get(INITIAL_SETTINGS_ACCESS_TO_TICKET_TYPES).toBuilder()
//                .groupTicketTypes(null)
//                .build();
//        var request = settingsTestHelper.convertEntityToDtoRequest(createdEntity, true);
//        given().
//                when().
//                headers(
//                        "Authorization",
//                        "Bearer " + itHelper.getTokens().get(user.getEmail())
//                )
//                .contentType(APPLICATION_JSON)
//                .body(request)
//                .post(SETTING_ACCESS_TO_TICKET_VIA_TICKET_TYPES)
//                .then()
//                .log().body()
//                .body(STATUS, equalTo(HttpStatus.BAD_REQUEST.value()));
//    }
//
//    @ParameterizedTest(name = "{index} User: {0}")
//    @MethodSource("getStreamAllUsers")
//    @Order(100)
//    void failedCreateByAllUsersIfFromNotInnerGroupAndNotHaveRoleAccountOwnerOrAdmin
//            (String userKey, User user) {
//        if (user.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString()) ||
//                (user.getRole().getName().equals(Roles.ADMIN.toString()) && user.getGroup().getIsInner())) {
//            return;
//        }
//        var createdEntity = itHelper.getSettingsAccessToTicketTypes().get(INITIAL_SETTINGS_ACCESS_TO_TICKET_TYPES);
//        var request = settingsTestHelper.convertEntityToDtoRequest(createdEntity, true);
//        given().
//                when().
//                headers(
//                        "Authorization",
//                        "Bearer " + itHelper.getTokens().get(user.getEmail())
//                )
//                .contentType(APPLICATION_JSON)
//                .body(request)
//                .post(SETTING_ACCESS_TO_TICKET_VIA_TICKET_TYPES)
//                .then()
//                .log().body()
//                .body(STATUS, equalTo(HttpStatus.FORBIDDEN.value()));
//    }
//
//    @Test
//    @Order(110)
//    void failedCreateByAnonymousUser() {
//        given().
//                when()
//                .contentType(APPLICATION_JSON)
//                .body(EMPTY_BODY)
//                .post(SETTING_ACCESS_TO_TICKET_VIA_TICKET_TYPES)
//                .then()
//                .log().body()
//                .statusCode(HttpStatus.UNAUTHORIZED.value());
//    }
//
//    @ParameterizedTest(name = "{index} User: {0}")
//    @MethodSource("getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin")
//    @Order(120)
//    void successUpdateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin(String userKey, User user) {
//        var updatedEntity = itHelper.getSettingsAccessToTicketTypes()
//                .get(INITIAL_SETTINGS_ACCESS_TO_TICKET_TYPES).toBuilder().build();
//        var request = settingsTestHelper.convertEntityToDtoRequest(updatedEntity, false);
//        var entityAfterUpdate = given().
//                when().
//                headers(
//                        "Authorization",
//                        "Bearer " + itHelper.getTokens().get(user.getEmail())
//                )
//                .contentType(APPLICATION_JSON)
//                .body(request)
//                .put(SETTING_ACCESS_TO_TICKET_VIA_TICKET_TYPES)
//                .then()
//                .log().body()
//                .statusCode(HttpStatus.OK.value())
//                .extract().response().as(SettingsAccessToTicketTypes.class);
//        itHelper.getSettingsAccessToTicketTypes().get(INITIAL_SETTINGS_ACCESS_TO_TICKET_TYPES)
//                .setVersion(entityAfterUpdate.getVersion());
//    }
//
//    @ParameterizedTest(name = "{index} User: {0}")
//    @MethodSource("getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin")
//    @Order(130)
//    void failedUpdateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin_whenKeysIsNotUnique(String userKey,
//                                                                                             User user) {
//        var updatedEntity = itHelper.getSettingsAccessToTicketTypes()
//                .get(INITIAL_SETTINGS_ACCESS_TO_TICKET_TYPES);
//        updatedEntity.setUser(user);
//        var request = settingsTestHelper.convertEntityToDtoRequest(updatedEntity, false);
//        var apiError = given().
//                when().
//                headers(
//                        "Authorization",
//                        "Bearer " + itHelper.getTokens().get(user.getEmail())
//                )
//                .contentType(APPLICATION_JSON)
//                .body(request)
//                .put(SETTING_ACCESS_TO_TICKET_VIA_TICKET_TYPES)
//                .then()
//                .log().body()
//                .body(STATUS, equalTo(HttpStatus.CONFLICT.value()))
//                .extract().response().as(ApiError.class);
//        assertEquals(
//                format(NOT_UNIQUE_MESSAGE, THIS_KEY_OF_SETTINGS_ACCOUNT_ID_GROUP_ID_USER_ID),
//                apiError.getErrors().get(NOT_UNIQUE_CODE).get(0).getMessage()
//        );
//    }
//
//    @ParameterizedTest(name = "{index} User: {0}")
//    @MethodSource("getStreamUsersFromInnerGroupWithRolesAccountOwnerAndAdmin")
//    @Order(140)
//    void failedUpdateByUsersFromInnerGroupsWithRolesAccountOwnerAndAdmin_whenGroupOfTicketTypesIsNull
//            (String userKey, User user) {
//        var updatedEntity = itHelper.getSettingsAccessToTicketTypes()
//                .get(INITIAL_SETTINGS_ACCESS_TO_TICKET_TYPES).toBuilder()
//                .groupTicketTypes(null)
//                .build();
//        var request = settingsTestHelper.convertEntityToDtoRequest(updatedEntity, false);
//        given().
//                when().
//                headers(
//                        "Authorization",
//                        "Bearer " + itHelper.getTokens().get(user.getEmail())
//                )
//                .contentType(APPLICATION_JSON)
//                .body(request)
//                .put(SETTING_ACCESS_TO_TICKET_VIA_TICKET_TYPES)
//                .then()
//                .log().body()
//                .body(STATUS, equalTo(HttpStatus.BAD_REQUEST.value()));
//    }
//
//    @ParameterizedTest(name = "{index} User: {0}")
//    @MethodSource("getStreamAllUsers")
//    @Order(150)
//    void failedUpdateByAllUsers_whenUserIsNotFromInnerGroupAndNotHaveRoleAccountOwnerOrAdmin(String userKey, User user) {
//        if (user.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString()) ||
//                (user.getRole().getName().equals(Roles.ADMIN.toString()) && user.getGroup().getIsInner())) {
//            return;
//        }
//        var updatedEntity = itHelper.getSettingsAccessToTicketTypes()
//                .get(INITIAL_SETTINGS_ACCESS_TO_TICKET_TYPES);
//        var request = settingsTestHelper.convertEntityToDtoRequest(updatedEntity, false);
//        given().
//                when().
//                headers(
//                        "Authorization",
//                        "Bearer " + itHelper.getTokens().get(user.getEmail())
//                )
//                .contentType(APPLICATION_JSON)
//                .body(request)
//                .put(SETTING_ACCESS_TO_TICKET_VIA_TICKET_TYPES)
//                .then()
//                .log().body()
//                .body(STATUS, equalTo(HttpStatus.FORBIDDEN.value()));
//    }
//
//    @Test
//    @Order(160)
//    void failedUpdateByAnonymousUser() {
//        given().
//                when()
//                .contentType(APPLICATION_JSON)
//                .body(EMPTY_BODY)
//                .put(SETTING_ACCESS_TO_TICKET_VIA_TICKET_TYPES)
//                .then()
//                .log().body()
//                .statusCode(HttpStatus.UNAUTHORIZED.value());
//    }

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
