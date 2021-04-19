package ru.itterminal.yanmas.IT.Specification;

import com.github.javafaker.Faker;
import io.restassured.RestAssured;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;
import ru.itterminal.yanmas.IT.util.ITHelper;
import ru.itterminal.yanmas.IT.util.ITTestConfig;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.dto.GroupFilterDto;
import ru.itterminal.yanmas.aau.model.dto.UserFilterDto;
import ru.itterminal.yanmas.aau.model.test.GroupTestHelper;
import ru.itterminal.yanmas.aau.repository.UserRepository;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.commons.model.dto.BaseFilterDto;
import ru.itterminal.yanmas.commons.model.filter.*;
import ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter;
import ru.itterminal.yanmas.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter;
import ru.itterminal.yanmas.commons.model.filter.NumberFilter.TypeComparisonForNumberFilter;
import ru.itterminal.yanmas.commons.model.filter.StringFilter.TypeComparisonForStringFilter;
import ru.itterminal.yanmas.security.jwt.JwtProvider;
import ru.itterminal.yanmas.tickets.model.GroupTicketTypes;
import ru.itterminal.yanmas.tickets.model.TicketTemplate;
import ru.itterminal.yanmas.tickets.model.TicketType;
import ru.itterminal.yanmas.tickets.model.dto.GroupTicketTypesDtoRequest;
import ru.itterminal.yanmas.tickets.model.dto.GroupTicketTypesFilterDto;
import ru.itterminal.yanmas.tickets.model.dto.TicketTemplateFilterDto;
import ru.itterminal.yanmas.tickets.model.test.TicketTypeTestHelper;

import java.util.List;
import java.util.Locale;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static io.restassured.RestAssured.given;
import static io.restassured.path.json.JsonPath.from;
import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static ru.itterminal.yanmas.IT.util.ITHelper.*;

@SuppressWarnings({"unused", "SimplifyStreamApiCallChains"})
@DataJpaTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class, JwtProvider.class})
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class SpecificationIT {

    public static final String NOT_VALID_COMPARISON = "not_valid_comparison";
    public static final String NOT_BLANK = "not_blank";
    public static final String EMPTY = "";
    public static final String IS_PREDEFINED_FOR_NEW_TICKET = "itIsPredefinedTicketTypeForNewTicket";
    public static final String TICKET_TYPE_1 = "ticketType_1";
    @SuppressWarnings("SpringJavaInjectionPointsAutowiringInspection")
    @Autowired
    private UserRepository userRepository;

    private static final ITHelper itHelper = new ITHelper();
    private final TicketTypeTestHelper ticketTypeTestHelper = new TicketTypeTestHelper();
    private final GroupTestHelper groupTestHelper = new GroupTestHelper();
    private final Faker fakerRU = new Faker(new Locale("ru", "RU"));
    private String token;

    @BeforeAll
    void beforeAll() {
        RestAssured.useRelaxedHTTPSValidation();
        RestAssured.baseURI = "https://localhost:8080";
        RestAssured.basePath = "/api/v1/";
        itHelper.createAndVerifyAccount(userRepository);
        token = itHelper.getTokens().get(itHelper.getAccountOwner().getEmail());
        itHelper.createInitialInnerAndOuterGroups(2, 2);
        itHelper.createInitialUsers();
        itHelper.createInitialTicketTemplates();
        itHelper.createInitialTicketType();
        itHelper.createInitialGroupTicketTypes();
        createTicketType();
        createGroupTicketTypes(List.of(itHelper.getTicketTypes().get(IS_PREDEFINED_FOR_NEW_TICKET), itHelper.getTicketTypes().get(TICKET_TYPE_1)), "groupTicketTypes_1");
        createGroupTicketTypes(List.of(itHelper.getTicketTypes().get(TICKET_TYPE_1)), "groupTicketTypes_2");
    }

    @ParameterizedTest()
    @MethodSource("getTrueFalse")
    @Order(10)
    void successBooleanFilterIsInnerTrue(Boolean isInner) {
        var expectedGroupList =
                itHelper.getOuterGroup().values().stream().collect(Collectors.toList());
        if (isInner) {
            expectedGroupList =
                    itHelper.getInnerGroup().values().stream().collect(Collectors.toList());
        }

        int expectedCountGroup = expectedGroupList.size();
        var filterDto = GroupFilterDto.builder()
                .isInner(BooleanFilter.builder()
                        .value(isInner)
                        .build())
                .build();
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + token)
                .contentType(APPLICATION_JSON)
                .body(filterDto)
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

    @Test
    @Order(20)
    void failedBooleanFilterValueIsNull() {
        var filterDto = GroupFilterDto.builder()
                .isInner(BooleanFilter.builder()
                        .value(null)
                        .build())
                .build();
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + token)
                .contentType(APPLICATION_JSON)
                .body(filterDto)
                .log().body()
                .get(GROUP)
                .then()
                .log().body()
                .statusCode(HttpStatus.BAD_REQUEST.value())
                .extract().response().asString();
    }

    @ParameterizedTest()
    @EnumSource(TypeComparisonForStringFilter.class)
    @Order(30)
    void successStringFilterIsValidDataFilter(TypeComparisonForStringFilter comparison) {
        var nameGroup = itHelper.getInnerGroup().get(INNER_GROUP_1).getName();
        var nameGroupRequest = switch (comparison) {
            case IS_EMPTY -> null;
            case IS_NOT_EMPTY -> "";
            case TEXT_CONTAINS -> nameGroup.substring(4).trim();
            case TEXT_NOT_CONTAINS -> nameGroup.substring(5).trim();
            case TEXT_STARTS_WITH -> nameGroup.substring(0, 4).trim();
            case TEXT_ENDS_WITH -> nameGroup.substring(6).trim();
            case TEXT_EQUALS -> nameGroup;
        };
        List<Group> expectedGroupList = switch (comparison) {
            case IS_EMPTY -> itHelper.getAllGroup().stream()
                    .filter(ng -> ng.getName() == null || ng.getName().isEmpty())
                    .collect(Collectors.toList());
            case IS_NOT_EMPTY -> itHelper.getAllGroup().stream()
                    .filter(ng -> ng.getName() != null && !ng.getName().isEmpty())
                    .collect(Collectors.toList());
            case TEXT_CONTAINS -> itHelper.getAllGroup().stream()
                    .filter(ng -> ng.getName().contains(nameGroupRequest))
                    .collect(Collectors.toList());
            case TEXT_NOT_CONTAINS -> itHelper.getAllGroup().stream()
                    .filter(ng -> !ng.getName().contains(nameGroupRequest))
                    .collect(Collectors.toList());
            case TEXT_STARTS_WITH -> itHelper.getAllGroup().stream()
                    .filter(ng -> ng.getName().startsWith(nameGroupRequest))
                    .collect(Collectors.toList());
            case TEXT_ENDS_WITH -> itHelper.getAllGroup().stream()
                    .filter(ng -> ng.getName().endsWith(nameGroupRequest))
                    .collect(Collectors.toList());
            case TEXT_EQUALS -> itHelper.getAllGroup().stream()
                    .filter(ng -> ng.getName().equals(nameGroupRequest))
                    .collect(Collectors.toList());
        };

        var filterDto = GroupFilterDto.builder()
                .name(StringFilter.builder()
                        .typeComparison(comparison.toString())
                        .value(nameGroupRequest)
                        .build())
                .build();
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + token)
                .contentType(APPLICATION_JSON)
                .body(filterDto)
                .get(GROUP)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        List<Group> actualGroupList = from(response).getList("content", Group.class);
        assertThat(expectedGroupList).usingElementComparatorIgnoringFields("account").containsExactlyInAnyOrderElementsOf(actualGroupList);
    }

    @ParameterizedTest(name = "{index} {2}")
    @MethodSource("getNotValidDataStringFilter")
    @Order(40)
    void failedStringFilterIsNotValidDataFilter(String endpoint, BaseFilterDto filterDto, String description) {
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + token)
                .contentType(APPLICATION_JSON)
                .body(filterDto)
                .get(endpoint)
                .then()
                .log().body()
                .statusCode(HttpStatus.BAD_REQUEST.value())
                .extract().response().asString();
    }

    @ParameterizedTest()
    @EnumSource(TypeComparisonForNumberFilter.class)
    @Order(50)
    void successNumberFilterIsValidDataFilter(TypeComparisonForNumberFilter comparison) {
        var valueTwo = itHelper.getTicketTemplates().get(TICKET_TEMPLATE_KEY + 1).getDateEnd();
        var valueOne = fakerRU.random().nextLong(valueTwo);
        List<TicketTemplate> expectedTicketTemplate = switch (comparison) {
            case IS_EMPTY -> itHelper.getTicketTemplates().values().stream()
                    .filter(tt -> tt.getDateStart() == null || tt.getDateStart() == 0L)
                    .collect(Collectors.toList());
            case IS_NOT_EMPTY -> itHelper.getTicketTemplates().values().stream()
                    .filter(tt -> tt.getDateStart() != null && tt.getDateStart() != 0L)
                    .collect(Collectors.toList());
            case GREATER_THAN -> itHelper.getTicketTemplates().values().stream()
                    .filter(tt -> tt.getDateStart() != null && tt.getDateStart() > valueOne)
                    .collect(Collectors.toList());
            case GREATER_THAN_OR_EQUAL_TO -> itHelper.getTicketTemplates().values().stream()
                    .filter(tt -> tt.getDateStart() != null && tt.getDateStart() >= valueOne)
                    .collect(Collectors.toList());
            case LESS_THAN -> itHelper.getTicketTemplates().values().stream()
                    .filter(tt -> tt.getDateStart() != null && tt.getDateStart() < valueOne)
                    .collect(Collectors.toList());
            case LESS_THAN_OR_EQUAL_TO -> itHelper.getTicketTemplates().values().stream()
                    .filter(tt -> tt.getDateStart() != null && tt.getDateStart() <= valueOne)
                    .collect(Collectors.toList());
            case IS_EQUAL_TO -> itHelper.getTicketTemplates().values().stream()
                    .filter(tt -> tt.getDateStart() != null && tt.getDateStart().equals(valueOne))
                    .collect(Collectors.toList());
            case IS_NOT_EQUAL_TO -> itHelper.getTicketTemplates().values().stream()
                    .filter(tt -> tt.getDateStart() != null && !tt.getDateStart().equals(valueOne))
                    .collect(Collectors.toList());
            case IS_BETWEEN_INCLUSIVE -> itHelper.getTicketTemplates().values().stream()
                    .filter(tt -> tt.getDateStart() != null && tt.getDateStart() >= valueOne && tt.getDateStart() <= valueTwo)
                    .collect(Collectors.toList());
            case IS_BETWEEN_EXCLUSION -> itHelper.getTicketTemplates().values().stream()
                    .filter(tt -> tt.getDateStart() != null && tt.getDateStart() > valueOne && tt.getDateStart() < valueTwo)
                    .collect(Collectors.toList());
            case IS_NOT_BETWEEN_INCLUSIVE -> itHelper.getTicketTemplates().values().stream()
                    .filter(tt -> tt.getDateStart() != null && (tt.getDateStart() <= valueOne || tt.getDateStart() >= valueTwo))
                    .collect(Collectors.toList());
            case IS_NOT_BETWEEN_EXCLUSION -> itHelper.getTicketTemplates().values().stream()
                    .filter(tt -> tt.getDateStart() != null && (tt.getDateStart() < valueOne || tt.getDateStart() > valueTwo))
                    .collect(Collectors.toList());
        };

        var filterDto = TicketTemplateFilterDto.builder()
                .dateStart(NumberFilter.builder()
                        .typeComparison(comparison.toString())
                        .valueOne(valueOne)
                        .valueTwo(valueTwo)
                        .build())
                .build();
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + token)
                .contentType(APPLICATION_JSON)
                .body(filterDto)
                .get(TICKET_TEMPLATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        List<TicketTemplate> actualTicketTemplateList = from(response).getList("content", TicketTemplate.class);
        assertThat(expectedTicketTemplate).usingElementComparatorIgnoringFields(IGNORE_FIELDS_FOR_COMPARE_TICKET_TEMPLATE)
                .containsExactlyInAnyOrderElementsOf(actualTicketTemplateList);
    }

    @ParameterizedTest(name = "{index} {2}")
    @MethodSource("getNotValidDataNumberFilter")
    @Order(60)
    void failedNumberFilterIsNotValidDataFilter(String endpoint, BaseFilterDto filterDto, String description) {
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + token)
                .contentType(APPLICATION_JSON)
                .body(filterDto)
                .get(endpoint)
                .then()
                .log().body()
                .statusCode(HttpStatus.BAD_REQUEST.value())
                .extract().response().asString();
    }

    @ParameterizedTest()
    @EnumSource(TypeComparisonForBaseEntityFilter.class)
    @Order(70)
    void successBaseEntityFilterIsValidDataFilter(TypeComparisonForBaseEntityFilter comparison) {
        var value = List.of(itHelper.getTicketTemplates().get(TICKET_TEMPLATE_KEY + 1).getTicketType().getId());
        List<TicketTemplate> expectedTicketTemplate = switch (comparison) {
            case IS_EMPTY -> itHelper.getTicketTemplates().values().stream()
                    .filter(tt -> tt.getTicketType() == null)
                    .collect(Collectors.toList());
            case IS_NOT_EMPTY -> itHelper.getTicketTemplates().values().stream()
                    .filter(tt -> tt.getTicketType() != null)
                    .collect(Collectors.toList());
            case EXIST_IN -> itHelper.getTicketTemplates().values().stream()
                    .filter(tt -> tt.getTicketType() != null && tt.getTicketType().getId().equals(value.get(0)))
                    .collect(Collectors.toList());
            case NOT_EXIST_IN -> itHelper.getTicketTemplates().values().stream()
                    .filter(tt -> tt.getTicketType() == null || !tt.getTicketType().getId().equals(value.get(0)))
                    .collect(Collectors.toList());
        };

        var filterDto = TicketTemplateFilterDto.builder()
                .ticketType(BaseEntityFilter.builder()
                        .typeComparison(comparison.toString())
                        .listOfIdEntities(value)
                        .build())
                .build();
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + token)
                .contentType(APPLICATION_JSON)
                .body(filterDto)
                .get(TICKET_TEMPLATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        List<TicketTemplate> actualTicketTemplateList = from(response).getList("content", TicketTemplate.class);
        assertThat(expectedTicketTemplate).usingElementComparatorIgnoringFields(IGNORE_FIELDS_FOR_COMPARE_TICKET_TEMPLATE)
                .containsExactlyInAnyOrderElementsOf(actualTicketTemplateList);
    }

    @ParameterizedTest(name = "{index} {2}")
    @MethodSource("getNotValidDataBaseEntityFilter")
    @Order(80)
    void failedBaseEntityFilterIsNotValidDataFilter(String endpoint, BaseFilterDto filterDto, String description) {
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + token)
                .contentType(APPLICATION_JSON)
                .body(filterDto)
                .get(endpoint)
                .then()
                .log().body()
                .statusCode(HttpStatus.BAD_REQUEST.value())
                .extract().response().asString();
    }

    @ParameterizedTest()
    @EnumSource(TypeComparisonForListOfBaseEntityFilter.class)
    @Order(90)
    void successListOfBaseEntityFilterIsValidDataFilter(TypeComparisonForListOfBaseEntityFilter comparison) {
        var groupTicketTypesList = List.of(itHelper.getGroupTicketTypes().get(INITIAL_GROUP_TICKET_TYPES));
        var ticketTypesList = itHelper.getGroupTicketTypes().get(INITIAL_GROUP_TICKET_TYPES).getTicketTypes();
        var listIdTicketTypes = ticketTypesList.stream()
                .map(BaseEntity::getId)
                .collect(Collectors.toList());
        List<GroupTicketTypes> expectedGroupTicketTypes = switch (comparison) {
            case IS_EMPTY -> itHelper.getGroupTicketTypes().values().stream()
                    .filter(tt -> tt.getTicketTypes() == null || tt.getTicketTypes().isEmpty())
                    .collect(Collectors.toList());
            case IS_NOT_EMPTY -> itHelper.getGroupTicketTypes().values().stream()
                    .filter(tt -> tt.getTicketTypes() != null && !tt.getTicketTypes().isEmpty())
                    .collect(Collectors.toList());
            case IS_EQUAL_TO -> groupTicketTypesList;
            case IS_NOT_EQUAL_TO -> itHelper.getGroupTicketTypes().values().stream()
                    .filter(tt -> !tt.getTicketTypes().equals(ticketTypesList))
                    .collect(Collectors.toList());
            case NOT_CONTAINS_ANY_IN_LIST -> itHelper.getGroupTicketTypes().values().stream()
                    .filter(tt -> !tt.getTicketTypes().contains(ticketTypesList.get(0)))
                    .collect(Collectors.toList());
            case CONTAINS_ANY_IN_LIST -> itHelper.getGroupTicketTypes().values().stream()
                    .filter(tt -> tt.getTicketTypes().contains(ticketTypesList.get(0)))
                    .collect(Collectors.toList());
            case CONTAINS_ALL_OF_LIST -> itHelper.getGroupTicketTypes().values().stream()
                    .filter(tt -> tt.getTicketTypes().containsAll(ticketTypesList))
                    .collect(Collectors.toList());
            case NOT_CONTAINS_ALL_OF_LIST -> itHelper.getGroupTicketTypes().values().stream()
                    .filter(tt -> !tt.getTicketTypes().containsAll(ticketTypesList))
                    .collect(Collectors.toList());
        };

        var filterDto = GroupTicketTypesFilterDto.builder()
                .ticketTypes(ListOfBaseEntityFilter.builder()
                        .typeComparison(comparison.toString())
                        .listOfIdEntities(listIdTicketTypes)
                        .build())
                .build();
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + token)
                .contentType(APPLICATION_JSON)
                .body(filterDto)
                .get(GROUP_TICKET_TYPES)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        List<GroupTicketTypes> actualGroupTicketTypesList = from(response).getList("content", GroupTicketTypes.class);
        assertThat(expectedGroupTicketTypes).usingElementComparatorIgnoringFields(IGNORE_FIELDS_FOR_COMPARE_TICKET_TEMPLATE)
                .containsExactlyInAnyOrderElementsOf(actualGroupTicketTypesList);
    }

    @ParameterizedTest(name = "{index} {2}")
    @MethodSource("getNotValidDataListOfBaseEntityFilter")
    @Order(80)
    void failedListOfBaseEntityFilterIsNotValidDataFilter(String endpoint, BaseFilterDto filterDto, String description) {
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + token)
                .contentType(APPLICATION_JSON)
                .body(filterDto)
                .get(endpoint)
                .then()
                .log().body()
                .statusCode(HttpStatus.BAD_REQUEST.value())
                .extract().response().asString();
    }

    private void createTicketType() {
        var ticketType = ticketTypeTestHelper.getRandomValidEntity();
        var ticketTypeDto = ticketTypeTestHelper.convertEntityToDtoRequest(ticketType, true);
        var actualTicketType = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + token
                )
                .contentType(APPLICATION_JSON)
                .body(ticketTypeDto)
                .post(TICKET_TYPE)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(TicketType.class);
        assertThat(List.of(ticketType))
                .usingElementComparatorOnFields("name", "outId", "comment")
                .containsExactlyInAnyOrderElementsOf(List.of(actualTicketType));
        itHelper.getTicketTypes().put(TICKET_TYPE_1, actualTicketType);
    }

    private void createGroupTicketTypes(List<TicketType> ticketTypeList, String name) {
        var ticketTypeIdList = ticketTypeList.stream()
                .map(BaseEntity::getId)
                .collect(Collectors.toList());
        var request = GroupTicketTypesDtoRequest.builder()
                .name(name)
                .ticketTypes(ticketTypeIdList)
                .build();
        var createdGroupTicketTypes = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + token
                )
                .contentType(APPLICATION_JSON)
                .body(request)
                .post(GROUP_TICKET_TYPES)
                .then()
                .log().body()
                .extract().response().as(GroupTicketTypes.class);
        createdGroupTicketTypes.setAccount(itHelper.getAccount());
        itHelper.getGroupTicketTypes().put(name, createdGroupTicketTypes);
    }

    private static Stream<Arguments> getNotValidDataListOfBaseEntityFilter() {
        return Stream.of(
                Arguments.of(GROUP_TICKET_TYPES,
                        buildGroupTicketTypesFilterDtoByTicketType(TypeComparisonForListOfBaseEntityFilter.IS_EQUAL_TO.toString(),
                                List.of()),
                        "IS_EQUAL_TO: listOfIdEntities is empty"),
                Arguments.of(GROUP_TICKET_TYPES,
                        buildGroupTicketTypesFilterDtoByTicketType(TypeComparisonForListOfBaseEntityFilter.IS_EQUAL_TO.toString(),
                                null),
                        "IS_EQUAL_TO: listOfIdEntities is null"),
                Arguments.of(GROUP_TICKET_TYPES,
                        buildGroupTicketTypesFilterDtoByTicketType(TypeComparisonForListOfBaseEntityFilter.IS_NOT_EQUAL_TO.toString(),
                                List.of()),
                        "IS_NOT_EQUAL_TO: listOfIdEntities is empty"),
                Arguments.of(GROUP_TICKET_TYPES,
                        buildGroupTicketTypesFilterDtoByTicketType(TypeComparisonForListOfBaseEntityFilter.IS_NOT_EQUAL_TO.toString(),
                                null),
                        "IS_NOT_EQUAL_TO: listOfIdEntities is null"),
                Arguments.of(GROUP_TICKET_TYPES,
                        buildGroupTicketTypesFilterDtoByTicketType(TypeComparisonForListOfBaseEntityFilter.NOT_CONTAINS_ANY_IN_LIST.toString(),
                                List.of()),
                        "NOT_CONTAINS_ANY_IN_LIST: listOfIdEntities is empty"),
                Arguments.of(GROUP_TICKET_TYPES,
                        buildGroupTicketTypesFilterDtoByTicketType(TypeComparisonForListOfBaseEntityFilter.NOT_CONTAINS_ANY_IN_LIST.toString(),
                                null),
                        "NOT_CONTAINS_ANY_IN_LIST: listOfIdEntities is null"),
                Arguments.of(GROUP_TICKET_TYPES,
                        buildGroupTicketTypesFilterDtoByTicketType(TypeComparisonForListOfBaseEntityFilter.CONTAINS_ANY_IN_LIST.toString(),
                                List.of()),
                        "CONTAINS_ANY_IN_LIST: listOfIdEntities is empty"),
                Arguments.of(GROUP_TICKET_TYPES,
                        buildGroupTicketTypesFilterDtoByTicketType(TypeComparisonForListOfBaseEntityFilter.CONTAINS_ANY_IN_LIST.toString(),
                                null),
                        "CONTAINS_ANY_IN_LIST: listOfIdEntities is null"),
                Arguments.of(GROUP_TICKET_TYPES,
                        buildGroupTicketTypesFilterDtoByTicketType(TypeComparisonForListOfBaseEntityFilter.CONTAINS_ALL_OF_LIST.toString(),
                                List.of()),
                        "CONTAINS_ALL_OF_LIST: listOfIdEntities is empty"),
                Arguments.of(GROUP_TICKET_TYPES,
                        buildGroupTicketTypesFilterDtoByTicketType(TypeComparisonForListOfBaseEntityFilter.CONTAINS_ALL_OF_LIST.toString(),
                                null),
                        "CONTAINS_ALL_OF_LIST: listOfIdEntities is null"),
                Arguments.of(GROUP_TICKET_TYPES,
                        buildGroupTicketTypesFilterDtoByTicketType(TypeComparisonForListOfBaseEntityFilter.NOT_CONTAINS_ALL_OF_LIST.toString(),
                                List.of()),
                        "NOT_CONTAINS_ALL_OF_LIST: listOfIdEntities is empty"),
                Arguments.of(GROUP_TICKET_TYPES,
                        buildGroupTicketTypesFilterDtoByTicketType(TypeComparisonForListOfBaseEntityFilter.NOT_CONTAINS_ALL_OF_LIST.toString(),
                                null),
                        "NOT_CONTAINS_ALL_OF_LIST: listOfIdEntities is null"),
                Arguments.of(GROUP_TICKET_TYPES,
                        buildGroupTicketTypesFilterDtoByTicketType(NOT_VALID_COMPARISON,
                                null),
                        "not valid comparison")
        );
    }

    private static Stream<Arguments> getNotValidDataBaseEntityFilter() {
        return Stream.of(
                Arguments.of(TICKET_TEMPLATE,
                        buildTicketTemplateFilterDtoByTicketType(TypeComparisonForBaseEntityFilter.EXIST_IN.toString(),
                                List.of()),
                        "EXIST_IN: listOfIdEntities is empty"),
                Arguments.of(TICKET_TEMPLATE,
                        buildTicketTemplateFilterDtoByTicketType(TypeComparisonForBaseEntityFilter.EXIST_IN.toString(),
                                null),
                        "EXIST_IN: listOfIdEntities is null"),
                Arguments.of(TICKET_TEMPLATE,
                        buildTicketTemplateFilterDtoByTicketType(TypeComparisonForBaseEntityFilter.NOT_EXIST_IN.toString(),
                                List.of()),
                        "NOT_EXIST_IN: listOfIdEntities is empty"),
                Arguments.of(TICKET_TEMPLATE,
                        buildTicketTemplateFilterDtoByTicketType(TypeComparisonForBaseEntityFilter.NOT_EXIST_IN.toString(),
                                null),
                        "NOT_EXIST_IN: listOfIdEntities is null"),
                Arguments.of(TICKET_TEMPLATE,
                        buildTicketTemplateFilterDtoByTicketType(NOT_VALID_COMPARISON,
                                null),
                        "not valid comparison")
        );
    }

    private static Stream<Arguments> getNotValidDataNumberFilter() {
        return Stream.of(
                Arguments.of(TICKET_TEMPLATE,
                        buildTicketTemplateFilterDtoByDateStart(TypeComparisonForNumberFilter.GREATER_THAN.toString(), null, null),
                        "GREATER_THAN: valueOne is null"),
                Arguments.of(TICKET_TEMPLATE,
                        buildTicketTemplateFilterDtoByDateStart(TypeComparisonForNumberFilter.GREATER_THAN_OR_EQUAL_TO.toString(), null, null),
                        "GREATER_THAN_OR_EQUAL_TO: valueOne is null"),
                Arguments.of(TICKET_TEMPLATE,
                        buildTicketTemplateFilterDtoByDateStart(TypeComparisonForNumberFilter.LESS_THAN.toString(), null, null),
                        "LESS_THAN: valueOne is null"),
                Arguments.of(TICKET_TEMPLATE,
                        buildTicketTemplateFilterDtoByDateStart(TypeComparisonForNumberFilter.LESS_THAN_OR_EQUAL_TO.toString(), null, null),
                        "LESS_THAN_OR_EQUAL_TO: valueOne is null"),
                Arguments.of(TICKET_TEMPLATE,
                        buildTicketTemplateFilterDtoByDateStart(TypeComparisonForNumberFilter.IS_EQUAL_TO.toString(), null, null),
                        "IS_EQUAL_TO: valueOne is null"),
                Arguments.of(TICKET_TEMPLATE,
                        buildTicketTemplateFilterDtoByDateStart(TypeComparisonForNumberFilter.IS_NOT_EQUAL_TO.toString(), null, null),
                        "IS_NOT_EQUAL_TO: valueOne is null"),
                Arguments.of(TICKET_TEMPLATE,
                        buildTicketTemplateFilterDtoByDateStart(TypeComparisonForNumberFilter.IS_BETWEEN_EXCLUSION.toString(), null, null),
                        "IS_BETWEEN_EXCLUSION: valueOne is null"),
                Arguments.of(TICKET_TEMPLATE,
                        buildTicketTemplateFilterDtoByDateStart(TypeComparisonForNumberFilter.IS_BETWEEN_INCLUSIVE.toString(), null, null),
                        "IS_BETWEEN_INCLUSIVE: valueOne is null"),
                Arguments.of(TICKET_TEMPLATE,
                        buildTicketTemplateFilterDtoByDateStart(TypeComparisonForNumberFilter.IS_NOT_BETWEEN_INCLUSIVE.toString(), null, null),
                        "IS_NOT_BETWEEN_INCLUSIVE: valueOne is null"),
                Arguments.of(TICKET_TEMPLATE,
                        buildTicketTemplateFilterDtoByDateStart(TypeComparisonForNumberFilter.IS_NOT_BETWEEN_EXCLUSION.toString(), null, null),
                        "IS_NOT_BETWEEN_EXCLUSION: valueOne is null"),
                Arguments.of(TICKET_TEMPLATE,
                        buildTicketTemplateFilterDtoByDateStart(TypeComparisonForNumberFilter.IS_BETWEEN_EXCLUSION.toString(), 1L, 0L),
                        "IS_BETWEEN_EXCLUSION: ValueTwo must be greater ValueOne"),
                Arguments.of(TICKET_TEMPLATE,
                        buildTicketTemplateFilterDtoByDateStart(TypeComparisonForNumberFilter.IS_BETWEEN_INCLUSIVE.toString(), 1L, 0L),
                        "IS_BETWEEN_INCLUSIVE: ValueTwo must be greater ValueOne"),
                Arguments.of(TICKET_TEMPLATE,
                        buildTicketTemplateFilterDtoByDateStart(TypeComparisonForNumberFilter.IS_NOT_BETWEEN_INCLUSIVE.toString(), 1L, 0L),
                        "IS_NOT_BETWEEN_INCLUSIVE: ValueTwo must be greater ValueOne"),
                Arguments.of(TICKET_TEMPLATE,
                        buildTicketTemplateFilterDtoByDateStart(TypeComparisonForNumberFilter.IS_NOT_BETWEEN_EXCLUSION.toString(), 1L, 0L),
                        "IS_NOT_BETWEEN_EXCLUSION: ValueTwo must be greater ValueOne"),
                Arguments.of(TICKET_TEMPLATE, buildTicketTemplateFilterDtoByDateStart(NOT_VALID_COMPARISON, null, null),
                        "not valid comparison")


        );
    }

    private static Stream<Arguments> getNotValidDataStringFilter() {
        return Stream.of(
                Arguments.of(GROUP, buildGroupFilterDtoByName(TypeComparisonForStringFilter.TEXT_CONTAINS.toString(), null),
                        "TEXT_CONTAINS: value is null"),
                Arguments.of(GROUP, buildGroupFilterDtoByName(TypeComparisonForStringFilter.TEXT_CONTAINS.toString(), EMPTY),
                        "TEXT_CONTAINS: value is empty"),
                Arguments.of(GROUP, buildGroupFilterDtoByName(TypeComparisonForStringFilter.TEXT_NOT_CONTAINS.toString(), null),
                        "TEXT_NOT_CONTAINS: value is null"),
                Arguments.of(GROUP, buildGroupFilterDtoByName(TypeComparisonForStringFilter.TEXT_NOT_CONTAINS.toString(), EMPTY),
                        "TEXT_NOT_CONTAINS: value is empty"),
                Arguments.of(GROUP, buildGroupFilterDtoByName(TypeComparisonForStringFilter.TEXT_STARTS_WITH.toString(), null),
                        "TEXT_STARTS_WITH: value is null"),
                Arguments.of(GROUP, buildGroupFilterDtoByName(TypeComparisonForStringFilter.TEXT_STARTS_WITH.toString(), EMPTY),
                        "TEXT_STARTS_WITH: value is empty"),
                Arguments.of(GROUP, buildGroupFilterDtoByName(TypeComparisonForStringFilter.TEXT_ENDS_WITH.toString(), null),
                        "TEXT_ENDS_WITH: value is null"),
                Arguments.of(GROUP, buildGroupFilterDtoByName(TypeComparisonForStringFilter.TEXT_ENDS_WITH.toString(), EMPTY),
                        "TEXT_ENDS_WITH: value is empty"),
                Arguments.of(GROUP, buildGroupFilterDtoByName(TypeComparisonForStringFilter.TEXT_EQUALS.toString(), null),
                        "TEXT_EQUALS: value is null"),
                Arguments.of(GROUP, buildGroupFilterDtoByName(TypeComparisonForStringFilter.TEXT_EQUALS.toString(), EMPTY),
                        "TEXT_EQUALS: value is empty"),
                Arguments.of(GROUP, buildGroupFilterDtoByName(TypeComparisonForStringFilter.TEXT_CONTAINS.toString(), StringUtils.repeat(NOT_BLANK, 15)),
                        "TEXT_CONTAINS: value length is greater"),
                Arguments.of(GROUP, buildGroupFilterDtoByName(TypeComparisonForStringFilter.TEXT_NOT_CONTAINS.toString(), StringUtils.repeat(NOT_BLANK, 15)),
                        "TEXT_NOT_CONTAINS: value length is greater"),
                Arguments.of(GROUP, buildGroupFilterDtoByName(TypeComparisonForStringFilter.TEXT_STARTS_WITH.toString(), StringUtils.repeat(NOT_BLANK, 15)),
                        "TEXT_STARTS_WITH: value length is greater"),
                Arguments.of(GROUP, buildGroupFilterDtoByName(TypeComparisonForStringFilter.TEXT_ENDS_WITH.toString(), StringUtils.repeat(NOT_BLANK, 15)),
                        "TEXT_ENDS_WITH: value length is greater"),
                Arguments.of(GROUP, buildGroupFilterDtoByName(TypeComparisonForStringFilter.TEXT_EQUALS.toString(), StringUtils.repeat(NOT_BLANK, 15)),
                        "TEXT_EQUALS: value length is greater"),
                Arguments.of(GROUP, buildGroupFilterDtoByName(NOT_VALID_COMPARISON, NOT_BLANK),
                        "not valid comparison"),
                Arguments.of(GROUP, buildInvalidGroupFilterDtoBySortByFields(),
                        "not valid sortByFields")
        );
    }

    private static Stream<Arguments> getTrueFalse() {
        return Stream.of(
                Arguments.of(true),
                Arguments.of(false)
        );
    }

    private static Stream<Arguments> getNameGroup() {
        return itHelper.getAllGroup().stream()
                .map(entry -> Arguments.of(entry.getName()));
    }

    private static GroupFilterDto buildGroupFilterDtoByName(String comparison, String value) {
        return GroupFilterDto.builder()
                .name(StringFilter.builder()
                        .typeComparison(comparison)
                        .value(value)
                        .build())
                .build();
    }

    private static GroupFilterDto buildInvalidGroupFilterDtoBySortByFields() {
        return GroupFilterDto.builder()
                .sortByFields(List.of(NOT_VALID_COMPARISON))
                .build();
    }

    private static TicketTemplateFilterDto buildTicketTemplateFilterDtoByDateStart(String comparison, Long valueOne, Long valueTwo) {
        return TicketTemplateFilterDto.builder()
                .dateStart(NumberFilter.builder()
                        .typeComparison(comparison)
                        .valueOne(valueOne)
                        .valueTwo(valueTwo)
                        .build())
                .build();
    }

    private static TicketTemplateFilterDto buildTicketTemplateFilterDtoByTicketType(String comparison, List<UUID> valueOne) {
        return TicketTemplateFilterDto.builder()
                .ticketType(BaseEntityFilter.builder()
                        .typeComparison(comparison)
                        .listOfIdEntities(valueOne)
                        .build())
                .build();
    }

    private static GroupTicketTypesFilterDto buildGroupTicketTypesFilterDtoByTicketType(String comparison, List<UUID> valueOne) {
        return GroupTicketTypesFilterDto.builder()
                .ticketTypes(ListOfBaseEntityFilter.builder()
                        .typeComparison(comparison)
                        .listOfIdEntities(valueOne)
                        .build())
                .build();
    }

    private static UserFilterDto buildUserFilterDtoByEmail(String comparison, String value) {
        return UserFilterDto.builder()
                .email(StringFilter.builder()
                        .typeComparison(comparison)
                        .value(value)
                        .build())
                .build();
    }
}
