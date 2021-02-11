package temp;

import io.restassured.RestAssured;
import lombok.SneakyThrows;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.springframework.context.annotation.Import;
import org.springframework.http.HttpStatus;
import org.springframework.security.test.context.support.WithUserDetails;
import ru.itterminal.botdesk.aau.model.test.AccountTestHelper;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;

import static io.restassured.RestAssured.given;
import static org.junit.jupiter.api.Assertions.assertTrue;

@Import(TestSecurityConfig.class)
class TempIT {

    private UserTestHelper userTestHelper = new UserTestHelper();
    private AccountTestHelper accountTestHelper = new AccountTestHelper();

    @Test
    void method_should_when() {
        assertTrue(true);
    }

    @Test
    void method_should_whend() {
        var user = userTestHelper.getRandomValidEntity();
        var account = accountTestHelper.getRandomValidEntity();
        RestAssured.useRelaxedHTTPSValidation();

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
                contentType("application/json").
                body(requestParams.toString()).
                post("https://localhost:8080/api/v1/create-account").
                then().log().body()
                .statusCode(HttpStatus.CREATED.value());
    }

    @Test
    void method_should_whendы() {

        RestAssured.useRelaxedHTTPSValidation();

        given().
                when().
                //contentType("application/json").
                param("email", "value1@gmail.com").
                get("https://localhost:8080/api/v1/auth/request-password-reset").
                then().log().body()
                .statusCode(HttpStatus.CREATED.value());
    }
}
