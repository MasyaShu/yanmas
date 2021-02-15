package ru.itterminal.botdesk.IT;

import io.restassured.RestAssured;
import io.restassured.response.Response;
import org.json.JSONObject;
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
import ru.itterminal.botdesk.aau.model.test.AccountTestHelper;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.aau.repository.UserRepository;

import java.util.HashMap;
import java.util.Map;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.equalTo;
import static ru.itterminal.botdesk.IT.util.ITHelper.*;

@SuppressWarnings("OptionalGetWithoutIsPresent")
@DataJpaTest
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class})
class TempIT {


    private final UserTestHelper userTestHelper = new UserTestHelper();
    private final AccountTestHelper accountTestHelper = new AccountTestHelper();

    @Autowired
    private UserRepository userRepository;


    @BeforeEach
    void setUp() {
        RestAssured.useRelaxedHTTPSValidation();
        RestAssured.baseURI = "https://localhost:8080";
        RestAssured.basePath = "/api/v1/";
    }

    @Test
    void createAccount_shouldCreated_whenValidData() {
        var user = userTestHelper.getRandomValidEntity();
        var account = accountTestHelper.getRandomValidEntity();

        ITHelper itHelper = new ITHelper();


        HashMap<String, String> jsonMap = new HashMap<>();
        jsonMap.put("name", account.getName());
        jsonMap.put("nameGroupAccountOwner", account.getName());
        jsonMap.put("passwordAccountOwner", user.getPassword());
        jsonMap.put("emailAccountOwner", user.getEmail());
        JSONObject jsonRequest = new JSONObject(jsonMap);


        given().
                when().
                contentType(APPLICATION_JSON).
                body(jsonRequest.toString()).
                post(CREATE_ACCOUNT).
                then()
                .body("name", equalTo(account.getName()))
                .body("deleted", equalTo(false))
                .body("version", equalTo(0))
                .log().body()
                .statusCode(HttpStatus.CREATED.value());

        HashMap<String, String> jsonMapSignIn = new HashMap<>();
        jsonMapSignIn.put("email", user.getEmail());
        jsonMapSignIn.put("password", user.getPassword());
        JSONObject jsonRequestSignIn = new JSONObject(jsonMapSignIn);

        given()
                .contentType(APPLICATION_JSON)
                .body(jsonRequestSignIn.toString())
                .post(SIGN_IN)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());

        User userFromDataBase = userRepository.getByEmail(user.getEmail()).get();

        given().
                when()
                .param("token", userFromDataBase.getEmailVerificationToken())
                .get(EMAIL_VERIFY)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value());

        Response response = given()
                .contentType(APPLICATION_JSON)
                .body(jsonRequestSignIn.toString())
                .post(SIGN_IN)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract()
                .response();



        String token = response.path("token");
        Map<String,String> mapToken = new HashMap<>();
        mapToken.put(user.getEmail(), token);
        itHelper.setTokens(mapToken);

        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + token)
                .contentType(APPLICATION_JSON)
                .body("{}")
                .get(ROLE)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value());

        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + token)
                .contentType(APPLICATION_JSON)
                .body("{}")
                .get(USER_GET_BY_FILTER)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value());

    }

    @Test
    void requestPasswordReset_shouldNotFound_whenEmailNotExist() {

        Response response = given().
                when()
                .param("email", NOT_EXIST_EMAIL)
                .get(REQUEST_PASSWORD_RESET)
                .then()
                .body("status", equalTo(404))
                .body("detail", equalTo(NOT_FOUND_USER_BY_EMAIL + NOT_EXIST_EMAIL))
                .log().body()
                .statusCode(HttpStatus.NOT_FOUND.value())
                .extract()
                .response();

        String type = response.path("type");
        System.out.print(type);
    }

}
