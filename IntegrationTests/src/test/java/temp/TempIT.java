package temp;

import io.restassured.RestAssured;
import io.restassured.response.Response;
import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.annotation.Import;
import org.springframework.http.HttpStatus;
import ru.itterminal.botdesk.aau.model.test.AccountTestHelper;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;

import java.util.HashMap;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.equalTo;

@Import(TestSecurityConfig.class)
class TempIT {

    public static final String CREATE_ACCOUNT = "create-account";
    public static final String APPLICATION_JSON = "application/json";
    public static final String REQUEST_PASSWORD_RESET = "auth/request-password-reset";
    public static final String NOT_EXIS_EMAIL = "notExisEmail@gmail.com";
    public static final String NOT_FOUND_USER_BY_EMAIL = "Not found user by email: ";
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
    }

    @Test
    void requestPasswordReset_shouldNotFound_whenEmailNotExist() {

        Response response = given().
                when()
                .param("email", NOT_EXIS_EMAIL)
                .get(REQUEST_PASSWORD_RESET)
                .then()
                .body("status", equalTo(404))
                .body("detail", equalTo(NOT_FOUND_USER_BY_EMAIL + NOT_EXIS_EMAIL))
                .log().body()
                .statusCode(HttpStatus.NOT_FOUND.value())
                .extract()
                .response();

        String type = response.path("type");
        System.out.print(type);
    }
}
