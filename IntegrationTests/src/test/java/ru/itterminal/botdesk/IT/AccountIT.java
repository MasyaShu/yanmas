package ru.itterminal.botdesk.IT;

import io.restassured.RestAssured;
import io.restassured.response.Response;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;
import ru.itterminal.botdesk.IT.util.ITHelper;
import ru.itterminal.botdesk.IT.util.ITTestConfig;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.aau.repository.UserRepository;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.equalTo;
import static ru.itterminal.botdesk.IT.util.ITHelper.*;

@DataJpaTest
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class})
class AccountIT {


    @Autowired
    private UserRepository userRepository;

    private final UserTestHelper userTestHelper = new UserTestHelper();
    private ITHelper itHelper;

    @BeforeEach
    void setUp() {
        RestAssured.useRelaxedHTTPSValidation();
        RestAssured.baseURI = "https://localhost:8080";
        RestAssured.basePath = "/api/v1/";
        itHelper = new ITHelper();
    }

    @Test
    void createAccount() {
        User user = userTestHelper.getRandomValidEntity();

        /*Creating an account with valid data
         * Создание аккаунта с валидными данными */
        String jsonCreateAccount = itHelper.createsJsonCreateAccount(user.getAccount().getName(), user.getGroup().getName(),
                user.getPassword(), user.getEmail());

        Response response = given().
                when().
                contentType(APPLICATION_JSON).
                body(jsonCreateAccount).
                post(CREATE_ACCOUNT).
                then()
                .body(NAME, equalTo(user.getAccount().getName()))
                .body(DELETED, equalTo(false))
                .body(VERSION, equalTo(0))
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract()
                .response();

        itHelper.builderAccount()
                .idAccount(response.path("id"))
                .nameAccount(response.path("name"))
                .deletedAccount(response.path("deleted"))
                .versionAccount(response.path("version"))
                .displayNameAccount(response.path("displayName"))
                .outIdAccount(response.path("outId"))
                .build();

        /*Trying to create an account again with the same data
         * Повторная попытка создать аккаунт с теми же данными*/
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


        /*Attempt to enter the account before email verification
        Попытка входа в аакаунт до верификации email*/
        String jsonSignIn = itHelper.builderJsonSignIn()
                .email(user.getEmail())
                .password(user.getPassword())
                .build();

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

        /*An attempt to verify an email with an invalid token
        * Попытка верифицировать email с невалидным токеном*/
        var userFromDataBase = userRepository.getByEmail(user.getEmail()).get();

        given().
                when()
                .param(TOKEN, userFromDataBase.getEmailVerificationToken() + "blablabla")
                .get(EMAIL_VERIFY)
                .then()
                .body(STATUS, equalTo(400))
                .body(TITLE, equalTo("JWT"))
                .body(DETAIL, equalTo("JWT signature does not match locally computed signature. JWT validity cannot be asserted and should not be trusted."))
                .log().body()
                .statusCode(HttpStatus.BAD_REQUEST.value());

        /*Successful account verification
        * Успешная верификация аккаунта*/
        given().
                when()
                .param(TOKEN, userFromDataBase.getEmailVerificationToken())
                .get(EMAIL_VERIFY)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value());

//        var verifyUserFromDataBase = userRepository.getByEmail(userFromDataBase.getEmail()).get();
//        assertTrue(verifyUserFromDataBase.getEmailVerificationStatus());
//        assertNull(verifyUserFromDataBase.getEmailVerificationToken());
    }
}
