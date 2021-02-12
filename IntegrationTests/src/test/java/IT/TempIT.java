package IT;

import io.restassured.RestAssured;
import io.restassured.response.Response;
import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import ru.itterminal.botdesk.aau.model.test.AccountTestHelper;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;

import java.util.HashMap;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertEquals;


class TempIT {

    public static final String CREATE_ACCOUNT = "create-account";
    public static final String APPLICATION_JSON = "application/json";
    public static final String REQUEST_PASSWORD_RESET = "auth/request-password-reset";
    public static final String NOT_EXIST_EMAIL = "notExisEmail@gmail.com";
    public static final String NOT_FOUND_USER_BY_EMAIL = "Not found user by email: ";
    public static final String SIGN_IN = "auth/signin";
   // public static final String EMAIL_VERIFY = "auth/email-verify";
    private final UserTestHelper userTestHelper = new UserTestHelper();
    private final AccountTestHelper accountTestHelper = new AccountTestHelper();

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

        HashMap<String, String> jsonMapSignIn= new HashMap<>();
        jsonMapSignIn.put("email", user.getEmail());
        jsonMapSignIn.put("password", user.getPassword());
        JSONObject jsonRequestSignIn = new JSONObject(jsonMapSignIn);

        given()
                .contentType(APPLICATION_JSON)
                .body(jsonRequestSignIn.toString())
                .post(SIGN_IN)
                .then()
                .statusCode(HttpStatus.FORBIDDEN.value());

      //  User userFromDataBase = userRepository.getByEmail(user.getEmail()).get();
//        var userServiceImpl =
//                (UserServiceImpl) appContext.getBean("userServiceImpl");
//        User userFromDataBase = userService.findByEmail(user.getEmail());
//
//        given().
//                when()
//                .param("token", userFromDataBase.getEmailVerificationToken())
//                .get(EMAIL_VERIFY)
//                .then()
//                .log().body()
//                .statusCode(HttpStatus.OK.value());


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

    @Test
    void testCreateAccount() {
        ITHelper itHelper = new ITHelper();
        itHelper.createAccount();
        assertEquals(0, (int) itHelper.getAccount().getVersion());
    }
}
