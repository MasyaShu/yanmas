package ru.itterminal.botdesk.IT;

import io.restassured.RestAssured;
import io.restassured.builder.RequestSpecBuilder;
import io.restassured.response.Response;
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
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.test.RoleTestHelper;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.aau.repository.UserRepository;
import ru.itterminal.botdesk.security.jwt.JwtProvider;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.repository.TicketStatusRepository;
import ru.itterminal.botdesk.tickets.repository.TicketTypeRepository;

import javax.persistence.EntityManager;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static io.restassured.RestAssured.given;
import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.*;
import static ru.itterminal.botdesk.IT.util.ITHelper.*;
import static ru.itterminal.botdesk.tickets.service.impl.TicketStatusServiceImpl.*;
import static ru.itterminal.botdesk.tickets.service.impl.TicketTypeServiceImpl.DEFAULT_TYPE;

@SuppressWarnings("OptionalGetWithoutIsPresent")
@DataJpaTest
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class, JwtProvider.class})
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@TestPropertySource(properties = {"jwt.token.secret=ksedtob", "jwt.token.expired=8640000", "jwt.token.prefix=Bearer"})
class AccountIT {


    public static final String ACCOUNT_CHECK_ACCESS = "account/check-access";
    @Autowired
    private JwtProvider jwtProvider;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private TicketTypeRepository ticketTypeRepository;

    @Autowired
    private TicketStatusRepository ticketStatusRepository;

    @Autowired
    private EntityManager entityManager;

    private static ITHelper itHelper;
    private static User anonymousUser;

    private final RoleTestHelper roleTestHelper = new RoleTestHelper();

    @BeforeAll
    static void beforeAll() {
        RestAssured.useRelaxedHTTPSValidation();
        RestAssured.baseURI = "https://localhost:8080";
        RestAssured.basePath = "/api/v1/";
        UserTestHelper userTestHelper = new UserTestHelper();
        itHelper = new ITHelper();
        anonymousUser = userTestHelper.getRandomValidEntity();
    }

    @Test
    @Order(10)
    void successCreatedAccount() {
        var jsonCreateAccount = itHelper.createAccountCreateDto(anonymousUser);
        var response = given().
                when().
                contentType(APPLICATION_JSON).
                body(jsonCreateAccount).
                post(CREATE_ACCOUNT).
                then()
                .body(NAME, equalTo(anonymousUser.getAccount().getName()))
                .body(DELETED, equalTo(false))
                .body(VERSION, equalTo(0))
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract()
                .response();

        itHelper.setAccount(response.as(Account.class));
    }

    @Test
    @Order(20)
    void failedCreateAccount_WhenEmailAlreadyOccupied() {
        var jsonCreateAccount = itHelper.createAccountCreateDto(anonymousUser);
        RequestSpecBuilder requestSpecBuilder = new RequestSpecBuilder();
        requestSpecBuilder.setBody(anonymousUser);
        given().
                when().
                contentType(APPLICATION_JSON).
                body(jsonCreateAccount).
                post(CREATE_ACCOUNT).
                then()
                .body(STATUS, equalTo(409))
                .body(TITLE, equalTo(VALIDATION_FAILED))
                .body(DETAIL, equalTo(INPUT_VALIDATION_FAILED))
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value());
    }

    @Test
    @Order(30)
    void AccessDenied_whenAccountIsNotActivated() {
        var jsonSignIn = itHelper.createAuthenticationRequestDto(anonymousUser);
        given()
                .contentType(APPLICATION_JSON)
                .body(jsonSignIn)
                .post(SIGN_IN)
                .then()
                .body(STATUS, equalTo(403))
                .body(TITLE, equalTo(AUTHENTICATION_FAILED))
                .body(DETAIL, equalTo(INVALID_USERNAME_OR_PASSWORD))
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(40)
    void failedVerifyEmail_whenTokenIsNotValid() {
        var notValidToken = jwtProvider.createToken(UUID.randomUUID());
        given().
                when()
                .param(TOKEN, notValidToken)
                .get(EMAIL_VERIFY)
                .then()
                .body(TYPE, equalTo(ENTITY_NOT_EXIST_EXCEPTION))
                .log().body()
                .statusCode(HttpStatus.NOT_FOUND.value());
    }

    @Test
    @Order(50)
    void successfulAccountVerification() {
        var userFromDataBase = userRepository.getByEmail(anonymousUser.getEmail()).get();
        given().
                when()
                .param(TOKEN, userFromDataBase.getEmailVerificationToken())
                .get(EMAIL_VERIFY)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value());
        entityManager.clear();
        var accountOwner = userRepository.getByEmail(anonymousUser.getEmail()).get();
        assertTrue(accountOwner.getEmailVerificationStatus());
        assertNull(accountOwner.getEmailVerificationToken());

        itHelper.setAccountOwner(accountOwner);

        var ticketType = ticketTypeRepository.findAllByAccountId(itHelper.getAccount().getId());
        assertEquals(DEFAULT_TYPE, ticketType.get(0).getName());
        assertEquals(1, ticketType.size());
        itHelper.getTicketTypes().put(IS_PREDEFINED_FOR_NEW_TICKET, ticketType.get(0));

        var ticketStatuses = ticketStatusRepository.findAllByAccountId(itHelper.getAccount().getId());
        var expectedTicketStatusNames = List.of(CANCELED, REOPENED, FINISHED, STARTED);
        var actualTicketStatusNames = ticketStatuses.stream()
                .map(TicketStatus::getName)
                .collect(Collectors.toList());
        assertEquals(4, ticketStatuses.size());
        assertThat(expectedTicketStatusNames).containsExactlyInAnyOrderElementsOf(actualTicketStatusNames);
        for(TicketStatus ticketStatus: ticketStatuses) {
            if(ticketStatus.getIsCanceledPredefined()) {
                itHelper.getTicketStatuses().put(IS_CANCELED_PREDEFINED, ticketStatus);
            }
            if(ticketStatus.getIsFinishedPredefined()) {
                itHelper.getTicketStatuses().put(IS_FINISHED_PREDEFINED, ticketStatus);
            }
            if(ticketStatus.getIsReopenedPredefined()) {
                itHelper.getTicketStatuses().put(IS_REOPENED_PREDEFINED, ticketStatus);
            }
            if(ticketStatus.getIsStartedPredefined()) {
                itHelper.getTicketStatuses().put(IS_STARTED_PREDEFINED, ticketStatus);
            }
        }
    }

    @Test
    @Order(60)
    void successfulSignInAccountOwner() {
        var jsonSignIn = itHelper.createAuthenticationRequestDto(anonymousUser);
        Response response = given()
                .contentType(APPLICATION_JSON)
                .body(jsonSignIn)
                .post(SIGN_IN)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract()
                .response();
        itHelper.getTokens().put(anonymousUser.getEmail(), response.path("token"));
        itHelper.activateAccount();

        itHelper.createInitialInnerAndOuterGroups(0, 1);
        var roles = roleTestHelper.setPredefinedValidEntityList();
        roles.remove(roleTestHelper.getRoleByName(Roles.ACCOUNT_OWNER.toString()));
        itHelper.createUsersForEachRoleInGroup(itHelper.getOuterGroup().get(OUTER_GROUP + 1), roles);
        itHelper.createUsersForEachRoleInGroup(itHelper.getInnerGroup().get(INNER_GROUP + 1), roles);
    }

    @Test
    @Order(70)
    void failedCreateAccount_whenUserIsNotAnonymous() {
        var jsonCreateAccount = itHelper.createAccountCreateDto(anonymousUser);
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(anonymousUser.getEmail()))
                .contentType(APPLICATION_JSON)
                .body(jsonCreateAccount)
                .post(CREATE_ACCOUNT)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(80)
    void getAccountByAccountOwner() {
        Response response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail()))
                .get(ACCOUNT)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response();
        assertEquals(itHelper.getAccount().getId().toString(), response.path(ID));
        assertEquals(itHelper.getAccount().getOutId(), response.path(OUT_ID));
        assertEquals(itHelper.getAccount().getDisplayName(), response.path(DISPLAY_NAME));
        assertEquals(itHelper.getAccount().getVersion(), response.path(VERSION));
        assertEquals(itHelper.getAccount().getDeleted(), response.path(DELETED));
        assertEquals(itHelper.getAccount().getName(), response.path(NAME));
    }

    @Test
    @Order(90)
    void getAccountByAnonymous() {
        given().
                when()
                .get(ACCOUNT)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(100)
    void successfulUpdateAccountByAccountOwner() {
        var updatedAccount = itHelper.getAccount().toBuilder().build();
        var updatedName = itHelper.getAccount().getName() + "Updated";
        var newVersion = updatedAccount.getVersion() + 1;
        updatedAccount.setName(updatedName);
        var accountDto = itHelper.createAccountDto(updatedAccount);
        var account = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail()))
                .contentType(APPLICATION_JSON)
                .body(accountDto)
                .put(ACCOUNT)
                .then()
                .body(NAME, equalTo(updatedName))
                .body(DISPLAY_NAME, equalTo(updatedName))
                .body(VERSION, equalTo(newVersion))
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract()
                .response().as(Account.class);
        itHelper.setAccount(account);
    }

    @ParameterizedTest
    @MethodSource("tokensUsersWhoDoNotHaveRightsToUpdateAccount")
    @Order(110)
    void failedUpdateAccount_whenUserNotAccountOwner(String userRole, String token) {
        var updatedAccount = itHelper.getAccount().toBuilder().build();
        var updatedName = userRole + itHelper.getAccount().getName();
        updatedAccount.setName(updatedName);
        var accountDto = itHelper.createAccountDto(updatedAccount);

        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + token)
                .contentType(APPLICATION_JSON)
                .body(accountDto)
                .put(ACCOUNT)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @ParameterizedTest
    @MethodSource("tokensUsersWhoDoNotHaveRightsToUpdateAccount")
    @Order(120)
    void failedCheckAccessUpdateAccount_whenUserNotAccountOwner(String userRole, String token) {
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + token)
                .contentType(APPLICATION_JSON)
                .put(ACCOUNT_CHECK_ACCESS)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    private static Stream<Arguments> tokensUsersWhoDoNotHaveRightsToUpdateAccount() {
        var emailAdminInnerGroup = itHelper.getAdminInnerGroup().get(ADMIN_INNER_GROUP + 1).getEmail();
        var emailExecutorInnerGroup =  itHelper.getExecutorInnerGroup().get(EXECUTOR_INNER_GROUP + 1).getEmail();
        var emailAuthorInnerGroup = itHelper.getAuthorInnerGroup().get(AUTHOR_INNER_GROUP + 1).getEmail();
        var emailObserverInnerGroup = itHelper.getObserverInnerGroup().get(OBSERVER_INNER_GROUP + 1).getEmail();
        var emailAdminOuterGroup = itHelper.getAdminOuterGroup().get(ADMIN_OUTER_GROUP + 1).getEmail();
        var emailExecutorOuterGroup =  itHelper.getExecutorOuterGroup().get(EXECUTOR_OUTER_GROUP + 1).getEmail();
        var emailAuthorOuterGroup = itHelper.getAuthorOuterGroup().get(AUTHOR_OUTER_GROUP + 1).getEmail();
        var emailObserverOuterGroup = itHelper.getObserverOuterGroup().get(OBSERVER_OUTER_GROUP + 1).getEmail();

        return Stream.of(
                Arguments.of(ADMIN_INNER_GROUP, itHelper.getTokens().get(emailAdminInnerGroup)),
                Arguments.of(EXECUTOR_INNER_GROUP, itHelper.getTokens().get(emailExecutorInnerGroup)),
                Arguments.of(AUTHOR_INNER_GROUP, itHelper.getTokens().get(emailAuthorInnerGroup)),
                Arguments.of(OBSERVER_INNER_GROUP, itHelper.getTokens().get(emailObserverInnerGroup)),
                Arguments.of(ADMIN_OUTER_GROUP, itHelper.getTokens().get(emailAdminOuterGroup)),
                Arguments.of(EXECUTOR_OUTER_GROUP, itHelper.getTokens().get(emailExecutorOuterGroup)),
                Arguments.of(AUTHOR_OUTER_GROUP, itHelper.getTokens().get(emailAuthorOuterGroup)),
                Arguments.of(OBSERVER_OUTER_GROUP, itHelper.getTokens().get(emailObserverOuterGroup))
        );
    }
}
