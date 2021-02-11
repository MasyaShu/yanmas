package temp;

import io.restassured.RestAssured;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.annotation.Import;
import org.springframework.http.HttpStatus;
import ru.itterminal.botdesk.aau.model.test.AccountTestHelper;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.equalTo;

@Import(TestSecurityConfig.class)
class TempIT {

    public static final String URL_API = "https://localhost:8080/api/v1/";
    public static final String CREATE_ACCOUNT = "create-account";
    public static final String APPLICATION_JSON = "application/json";
    private final UserTestHelper userTestHelper = new UserTestHelper();
    private final AccountTestHelper accountTestHelper = new AccountTestHelper();

    @BeforeEach
    void setUp() {
        RestAssured.useRelaxedHTTPSValidation();
    }

    @Test
    void createAccount_shouldCreated_whenValidData() {
        var user = userTestHelper.getRandomValidEntity();
        var account = accountTestHelper.getRandomValidEntity();

        JSONObject requestParams = null;
        try {
            requestParams = new JSONObject()
                    .put("name", account.getName())
                    .put("nameGroupAccountOwner", account.getName())
                    .put("passwordAccountOwner", user.getPassword())
                    .put("emailAccountOwner", user.getEmail());
        } catch (JSONException e) {
            e.printStackTrace();
        }


        given().
                when().
                contentType(APPLICATION_JSON).
                body(requestParams.toString()).
                post(URL_API + CREATE_ACCOUNT).
                then()
                .body("name", equalTo(account.getName()))
                .body("deleted", equalTo(false))
                .body("version", equalTo(0))
                .log().body()
                .statusCode(HttpStatus.CREATED.value());
    }

    @Test
    void requestPasswordReset_shouldNotFound_whendEmailNotExist() {

        RestAssured.useRelaxedHTTPSValidation();

        given().
                when().
                //contentType("application/json").
                        param("email", "value1@gmail.com").
                get("https://localhost:8080/api/v1/auth/request-password-reset").
                then().log().body()
                .statusCode(HttpStatus.NOT_FOUND.value());
    }
}
