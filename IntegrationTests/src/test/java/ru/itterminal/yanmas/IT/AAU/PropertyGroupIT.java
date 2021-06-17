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
import org.springframework.test.context.TestPropertySource;
import ru.itterminal.yanmas.IT.util.ITHelper;
import ru.itterminal.yanmas.IT.util.ITTestConfig;
import ru.itterminal.yanmas.aau.model.PropertyGroup;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.model.dto.PropertyGroupDto;
import ru.itterminal.yanmas.aau.model.dto.PropertyGroupFilterDto;
import ru.itterminal.yanmas.aau.repository.UserRepository;
import ru.itterminal.yanmas.commons.model.filter.NumberFilter;
import ru.itterminal.yanmas.commons.model.filter.StringFilter;
import ru.itterminal.yanmas.security.jwt.JwtProvider;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Stream;

import static io.restassured.RestAssured.given;
import static io.restassured.path.json.JsonPath.from;
import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static ru.itterminal.yanmas.IT.util.ITHelper.*;
import static ru.itterminal.yanmas.commons.model.filter.NumberFilter.TypeComparisonForNumberFilter.IS_EQUAL_TO;
import static ru.itterminal.yanmas.commons.model.filter.StringFilter.TypeComparisonForStringFilter.TEXT_EQUALS;

@SuppressWarnings("unused")
@DataJpaTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class, JwtProvider.class, UserRepository.class})
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@TestPropertySource(properties = {"jwt.token.secret=ksedtob", "jwt.token.expired=8640000", "jwt.token.prefix=Bearer"})
class PropertyGroupIT {

    public static final String NEW_NAME = "New name";
    @Autowired
    private UserRepository userRepository;

    private static final ITHelper itHelper = new ITHelper();
    private PropertyGroupDto createdPropertyGroupDto;

    @BeforeAll
    void beforeAll() {
        RestAssured.useRelaxedHTTPSValidation();
        RestAssured.baseURI = "https://localhost:8080";
        RestAssured.basePath = "/api/v1/";
        itHelper.createAndVerifyAccount(userRepository);
        itHelper.createInitialInnerAndOuterGroups(1, 2);
        itHelper.createInitialUsers();
        itHelper.createInitialPropertyGroups();
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllInitialUsers")
    @Order(10)
    void SuccessGetByFilterWhenFilterIsEmpty(String userKey, User currentUser) {
        var response = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .get(PROPERTY_GROUP)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        var actualPropertyGroupList = from(response).getList(CONTENT, PropertyGroupDto.class);
        var expectedPropertyGroupList = new ArrayList<>(itHelper.getPropertyGroup().values());
        assertEquals(expectedPropertyGroupList.size(), actualPropertyGroupList.size());
        assertThat(expectedPropertyGroupList).containsAll(actualPropertyGroupList);
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllInitialUsers")
    @Order(20)
    void SuccessGetByFilterWhenFilterIsNotEmpty(String userKey, User currentUser) {
        var expectedPropertyGroupDto = itHelper.getPropertyGroup().get(INITIAL_PROPERTY_GROUPS_1);
        var filter = PropertyGroupFilterDto.builder()
                .name(StringFilter.builder()
                              .value(expectedPropertyGroupDto.getName())
                              .typeComparison(TEXT_EQUALS.toString())
                              .build())
                .description(StringFilter.builder()
                                     .value(expectedPropertyGroupDto.getDescription())
                                     .typeComparison(TEXT_EQUALS.toString())
                                     .build())
                .orderView(NumberFilter.builder()
                                   .valueOne(expectedPropertyGroupDto.getOrderView())
                                   .typeComparison(IS_EQUAL_TO.toString())
                                   .build())
                .build();
        var response = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(filter)
                .get(PROPERTY_GROUP)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        var actualPropertyGroupList = from(response).getList(CONTENT, PropertyGroupDto.class);
        var expectedPropertyGroupList = List.of(expectedPropertyGroupDto);
        assertEquals(expectedPropertyGroupList.size(), actualPropertyGroupList.size());
        assertThat(expectedPropertyGroupList).containsAll(actualPropertyGroupList);
    }

    @Test
    @Order(30)
    void UnauthorizedHttpStatusForAnonymousUserGetByFilter() {
        given().
                when()
                .body(EMPTY_BODY)
                .get(PROPERTY_GROUP)
                .then()
                .log().body()
                .statusCode(HttpStatus.UNAUTHORIZED.value());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllInitialUsers")
    @Order(40)
    void SuccessGetByIdAllInitialPropertyGroups(String userKey, User currentUser) {
        var initialPropertyGroups = new ArrayList<>(itHelper.getPropertyGroup().values());
        for (PropertyGroupDto expectedPropertyGroup : initialPropertyGroups) {
            var actualPropertyGroup = given().
                    when().
                    headers(
                            "Authorization",
                            "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                    )
                    .contentType(APPLICATION_JSON)
                    .pathParam(ID, expectedPropertyGroup.getId())
                    .get(PROPERTY_GROUP_BY_ID)
                    .then()
                    .log().body()
                    .statusCode(HttpStatus.OK.value())
                    .extract().response().as(PropertyGroupDto.class);
            assertEquals(expectedPropertyGroup, actualPropertyGroup);
        }
    }

    @Test
    @Order(50)
    void UnauthorizedHttpStatusForAnonymousUserGetById() { //NOSONAR
        given().
                when()
                .body(EMPTY_BODY)
                .pathParam(ID, UUID.randomUUID())
                .get(PROPERTY_GROUP_BY_ID)
                .then()
                .log().body()
                .statusCode(HttpStatus.UNAUTHORIZED.value());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamInitialAccountOwnerAndAdminFromInnerGroup")
    @Order(60)
    void successCreate(String userKey, User currentUser) {
        var expectedPropertyGroupDto = PropertyGroupDto.builder()
                .name(itHelper.getFaker().name().name())
                .description(itHelper.getFaker().lorem().sentence(4))
                .build();
        var actualPropertyGroupDto = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(expectedPropertyGroupDto)
                .post(PROPERTY_GROUP)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(PropertyGroupDto.class);
        createdPropertyGroupDto = actualPropertyGroupDto;
        assertThat(actualPropertyGroupDto).usingRecursiveComparison().ignoringFields(IGNORE_FIELDS_OF_BASE_ENTITY_FOR_COMPARE).isEqualTo(expectedPropertyGroupDto);
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllInitialUsers")
    @Order(70)
    void AccessDeniedForCreateIfCurrentUserIsNotAccountOwnerOrAdminFromInnerGroup(String userKey, User currentUser) {
        if (currentUser.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString()) ||
                (currentUser.getRole().getName().equals(Roles.ADMIN.toString()) && currentUser.getGroup().getIsInner())) {
            return;
        }
        var expectedPropertyGroupDto = PropertyGroupDto.builder()
                .name(itHelper.getFaker().name().name())
                .description(itHelper.getFaker().lorem().sentence(4))
                .build();
        var actualPropertyGroupDto = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(expectedPropertyGroupDto)
                .post(PROPERTY_GROUP)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(80)
    void BadRequestBeforeCreateBecauseNameIsNull() {
        var expectedPropertyGroupDto = new PropertyGroupDto();
        given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(expectedPropertyGroupDto)
                .post(PROPERTY_GROUP)
                .then()
                .log().body()
                .body(containsString("must not be null"))
                .statusCode(HttpStatus.BAD_REQUEST.value());
    }

    @Test
    @Order(90)
    void BadRequestBeforeCreateBecauseNameIsMoreThan256Chapters() {
        var expectedPropertyGroupDto = PropertyGroupDto.builder()
                .name(itHelper.getFaker().lorem().characters(300))
                .build();
        given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(expectedPropertyGroupDto)
                .post(PROPERTY_GROUP)
                .then()
                .log().body()
                .body(containsString("size must be between 1 and 256"))
                .statusCode(HttpStatus.BAD_REQUEST.value());
    }

    @Test
    @Order(100)
    void UnauthorizedHttpStatusForAnonymousUserCreate() {
        given().
                when()
                .body(EMPTY_BODY)
                .post(PROPERTY_GROUP)
                .then()
                .log().body()
                .statusCode(HttpStatus.UNAUTHORIZED.value());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamInitialAccountOwnerAndAdminFromInnerGroup")
    @Order(110)
    void successUpdate(String userKey, User currentUser) {
        createdPropertyGroupDto.setName(NEW_NAME);
        createdPropertyGroupDto.setDisplayName(null);
        var actualPropertyGroupDto = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(createdPropertyGroupDto)
                .put(PROPERTY_GROUP)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(PropertyGroupDto.class);
        assertThat(actualPropertyGroupDto).usingRecursiveComparison().ignoringFields(IGNORE_FIELDS_OF_BASE_ENTITY_FOR_COMPARE).isEqualTo(createdPropertyGroupDto);
        createdPropertyGroupDto.setVersion(actualPropertyGroupDto.getVersion());
    }

    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllInitialUsers")
    @Order(120)
    void AccessDeniedForUpdateIfCurrentUserIsNotAccountOwnerOrAdminFromInnerGroup(String userKey, User currentUser) {
        if (currentUser.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString()) ||
                (currentUser.getRole().getName().equals(Roles.ADMIN.toString()) && currentUser.getGroup().getIsInner())) {
            return;
        }
        var actualPropertyGroupDto = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(createdPropertyGroupDto)
                .put(PROPERTY_GROUP)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(130)
    void BadRequestBeforeUpdateBecauseNameIsNull() {
        var expectedPropertyGroupDto = new PropertyGroupDto();
        given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(expectedPropertyGroupDto)
                .put(PROPERTY_GROUP)
                .then()
                .log().body()
                .body(containsString("must not be null"))
                .statusCode(HttpStatus.BAD_REQUEST.value());
    }

    @Test
    @Order(140)
    void BadRequestBeforeUpdateBecauseNameIsMoreThan256Chapters() {
        var expectedPropertyGroupDto = PropertyGroupDto.builder()
                .name(itHelper.getFaker().lorem().characters(300))
                .build();
        given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(expectedPropertyGroupDto)
                .put(PROPERTY_GROUP)
                .then()
                .log().body()
                .body(containsString("size must be between 1 and 256"))
                .statusCode(HttpStatus.BAD_REQUEST.value());
    }

    @Test
    @Order(150)
    void UnauthorizedHttpStatusForAnonymousUserUpdate() {
        given().
                when()
                .body(EMPTY_BODY)
                .put(PROPERTY_GROUP)
                .then()
                .log().body()
                .statusCode(HttpStatus.UNAUTHORIZED.value());
    }

    private static Stream<Arguments> getStreamAllInitialUsers() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(
                        ACCOUNT_OWNER,
                        ADMIN,
                        EXECUTOR,
                        AUTHOR,
                        OBSERVER
                )
        );
        return itHelper.getStreamUsers(roles, null);
    }

    private static Stream<Arguments> getStreamInitialAccountOwnerAndAdminFromInnerGroup() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(
                        ACCOUNT_OWNER,
                        ADMIN
                )
        );
        return itHelper.getStreamUsers(roles, true);
    }
}
