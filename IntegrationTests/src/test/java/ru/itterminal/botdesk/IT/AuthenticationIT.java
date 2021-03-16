package ru.itterminal.botdesk.IT;

import io.restassured.RestAssured;
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
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.dto.ResetPasswordDto;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.aau.repository.UserRepository;
import ru.itterminal.botdesk.security.jwt.JwtProvider;

import java.util.List;
import java.util.UUID;
import java.util.stream.Stream;

import static io.restassured.RestAssured.given;
import static ru.itterminal.botdesk.IT.util.ITHelper.*;

@DataJpaTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class, JwtProvider.class})
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@TestPropertySource(properties = {"jwt.token.secret=ksedtob", "jwt.token.expired=8640000", "jwt.token.prefix=Bearer"})
class AuthenticationIT {
    public static final String REQUEST_EMAIL_UPDATE = "auth/request-email-update";
    public static final String EMAIL_UPDATE = "auth/email-update";
    @Autowired
    private JwtProvider jwtProvider;

    @Autowired
    private UserRepository userRepository;

    private static ITHelper itHelper;
    private final UserTestHelper userTestHelper = new UserTestHelper();
    private String oldAccountOwnerPassword;
    private String newEmailAccountOwner;
    private String tokenForUpdateEmail;

    public static final String PASSWORD_RESET = "auth/password-reset";
    public static final String REQUEST_PASSWORD_RESET = "auth/request-password-reset";
    public static final String EMAIL = "email";

    @BeforeAll
    void beforeAll() {
        RestAssured.useRelaxedHTTPSValidation();
        RestAssured.baseURI = "https://localhost:8080";
        RestAssured.basePath = "/api/v1/";
        itHelper = new ITHelper();
        itHelper.createAndVerifyAccount(userRepository);
        itHelper.createInitialInnerAndOuterGroups(1, 2);
        itHelper.createInitialUsers();
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getAllUsers")
    @Order(10)
    void failedPasswordResetByAuthorizedUsers(String userKey, User user) {
        var expectedGroupList = itHelper.getAllGroup();
        int expectedCountGroup = expectedGroupList.size();
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(user.getEmail()))
                .param(EMAIL, user.getEmail())
                .get(REQUEST_PASSWORD_RESET)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(20)
    void failedPasswordResetByAnonymousUserWhenEmailNotExist() {
        var email = userTestHelper.getRandomValidEntity().getEmail();
        given().
                when()
                .param(EMAIL, email)
                .get(REQUEST_PASSWORD_RESET)
                .then()
                .log().body()
                .statusCode(HttpStatus.NOT_FOUND.value());
    }

    @SuppressWarnings("OptionalGetWithoutIsPresent")
    @Test
    @Order(30)
    void successPasswordResetByAnonymousUserWhenEmailExist() {
        var userAccountOwner = itHelper.getAccountOwner();
        given().
                when()
                .param(EMAIL, userAccountOwner.getEmail())
                .get(REQUEST_PASSWORD_RESET)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value());
        var userAccountOwnerFromDataBase = userRepository.getByEmail(userAccountOwner.getEmail()).get();
        itHelper.getAccountOwner().setPasswordResetToken(userAccountOwnerFromDataBase.getPasswordResetToken());
    }

    @Test
    @Order(40)
    void successSignInByUserAccountOwnerAfterAPasswordResetRequest() {
        successSignInAccountOwner();
    }

    @Test
    @Order(50)
    void failedPasswordResetWhenTokenIsNotValid() {
        var userAccountOwner = itHelper.getAccountOwner();
        var notValidToken = jwtProvider.createTokenWithUserId(UUID.randomUUID());
        ResetPasswordDto resetPasswordDto = ResetPasswordDto.builder()
                .token(notValidToken)
                .password(userAccountOwner.getPassword())
                .build();
        given()
                .contentType(APPLICATION_JSON)
                .body(resetPasswordDto)
                .get("auth/password-reset")
                .then()
                .log().body()
                .statusCode(HttpStatus.NOT_FOUND.value());
    }

    @Test
    @Order(60)
    void failedPasswordResetWhenPasswordIsNotValid() {
        var userAccountOwner = itHelper.getAccountOwner();
        ResetPasswordDto resetPasswordDto = ResetPasswordDto.builder()
                .token(userAccountOwner.getPasswordResetToken())
                .password("123456")
                .build();
        given()
                .contentType(APPLICATION_JSON)
                .body(resetPasswordDto)
                .get("auth/password-reset")
                .then()
                .log().body()
                .statusCode(HttpStatus.BAD_REQUEST.value());
    }

    @Test
    @Order(70)
    void successPasswordReset() {
        var newPasswordAccountOwner = userTestHelper.getRandomValidEntity().getPassword();
        var userAccountOwner = itHelper.getAccountOwner();
        ResetPasswordDto resetPasswordDto = ResetPasswordDto.builder()
                .token(userAccountOwner.getPasswordResetToken())
                .password(newPasswordAccountOwner)
                .build();
        given()
                .contentType(APPLICATION_JSON)
                .body(resetPasswordDto)
                .get(PASSWORD_RESET)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value());
        oldAccountOwnerPassword = userAccountOwner.getPassword();
        userAccountOwner.setPassword(newPasswordAccountOwner);
    }

    @Test
    @Order(80)
    void failedSignInByUserAccountOwnerWithOldPassword() {
        var userAccountOwner = itHelper.getAccountOwner();
        var jsonSignIn = userTestHelper.convertUserToAuthenticationRequestDto(userAccountOwner);
        jsonSignIn.setPassword(oldAccountOwnerPassword);
        given()
                .contentType(APPLICATION_JSON)
                .body(jsonSignIn)
                .post(SIGN_IN)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(90)
    void successSignInByUserAccountOwnerWithNewPassword() {
        successSignInAccountOwner();
    }

    @Test
    @Order(100)
    void failedRequestEmailUpdateByAnonymousUser() {
        var newEmail = userTestHelper.getRandomValidEntity().getEmail();
        given()
                .param(EMAIL, newEmail)
                .post(REQUEST_EMAIL_UPDATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getTokensOfAllUsersNotAccountOwner")
    @Order(110)
    void failedRequestEmailUpdateByAllUserNotAccountOwner(String userKey, String token) {
        var newEmail = userTestHelper.getRandomValidEntity().getEmail();
        given()
                .headers(
                        "Authorization",
                        "Bearer " + token
                )
                .param("newEmail", newEmail)
                .get(REQUEST_EMAIL_UPDATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(120)
    void failedRequestEmailUpdateByAccountOwnerWhenEmailNotUnique() {
        var emailAccountOwner = itHelper.getAccountOwner().getEmail();
        given()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(emailAccountOwner)
                )
                .param("newEmail", emailAccountOwner)
                .get(REQUEST_EMAIL_UPDATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value());
    }

    @Test
    @Order(130)
    void successRequestEmailUpdateByAccountOwner() {
        newEmailAccountOwner = userTestHelper.getRandomValidEntity().getEmail();
        given()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .param("newEmail", newEmailAccountOwner)
                .get(REQUEST_EMAIL_UPDATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value());
    }

    @SuppressWarnings("OptionalGetWithoutIsPresent")
    @Test
    @Order(140)
    void successRepeatedRequestEmailUpdateByAccountOwner() {
        var emailAccountOwner = itHelper.getAccountOwner().getEmail();
        newEmailAccountOwner = userTestHelper.getRandomValidEntity().getEmail();
        given()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(emailAccountOwner)
                )
                .param("newEmail", newEmailAccountOwner)
                .get(REQUEST_EMAIL_UPDATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value());
        tokenForUpdateEmail = userRepository.getByEmail(emailAccountOwner).get().getEmailVerificationToken();
    }

    @Test
    @Order(150)
    void failedSignInByUserAccountOwnerWithNewEmail() {
        var userAccountOwner = itHelper.getAccountOwner();
        var jsonSignIn = userTestHelper.convertUserToAuthenticationRequestDto(userAccountOwner);
        jsonSignIn.setEmail(newEmailAccountOwner);
        given()
                .contentType(APPLICATION_JSON)
                .body(jsonSignIn)
                .post(SIGN_IN)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(160)
    void successSignInByUserAccountOwnerWithOldEmail() {
        successSignInAccountOwner();
    }

    @Test
    @Order(170)
    void failedEmailUpdateWhenTokenIsNotValid() {
        var notValidToken = jwtProvider.createTokenWithUserId(UUID.randomUUID());
        given()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .param(TOKEN, notValidToken)
                .get(EMAIL_UPDATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.BAD_REQUEST.value());
    }

    @Test
    @Order(180)
    void failedEmailUpdateWhenAnonymousUser() {
        var notValidToken = jwtProvider.createTokenWithUserId(UUID.randomUUID());
        given()
                .param(TOKEN, notValidToken)
                .get(EMAIL_UPDATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getTokensOfAllUsersNotAccountOwner")
    @Order(190)
    void failedEmailUpdateByAllUserNotAccountOwner(String userKey, String token) {
        var newEmail = userTestHelper.getRandomValidEntity().getEmail();
        given()
                .headers(
                        "Authorization",
                        "Bearer " + token
                )
                .param(TOKEN, tokenForUpdateEmail)
                .get(EMAIL_UPDATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(200)
    void failedEmailUpdateWhenEmailIsNotUnique() {
        var userForUpdate = itHelper.getAdminInnerGroup().get(ADMIN_INNER_GROUP + 1);
        var oldEmailUserForUpdate = userForUpdate.getEmail();
        userForUpdate.setEmail(newEmailAccountOwner);
        itHelper.updateUser(userForUpdate);
        given()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .param(TOKEN, tokenForUpdateEmail)
                .get(EMAIL_UPDATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value());
        userForUpdate.setEmail(oldEmailUserForUpdate);
        itHelper.updateUser(userForUpdate);
    }

    @Test
    @Order(210)
    void successEmailUpdateWhenEmailIsUnique() {
        given()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .param(TOKEN, tokenForUpdateEmail)
                .get(EMAIL_UPDATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value());
    }

    @Test
    @Order(220)
    void successRequestEmailUpdateByAccountOwnerWhenOldAuthenticationToken() {
        var randomEmail = userTestHelper.getRandomValidEntity().getEmail();
        given()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .param("newEmail", randomEmail)
                .get(REQUEST_EMAIL_UPDATE)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value());
    }

    @Test
    @Order(230)
    void failedSignInByUserAccountOwnerWithOldEmail() {
        var userAccountOwner = itHelper.getAccountOwner();
        var jsonSignIn = userTestHelper.convertUserToAuthenticationRequestDto(userAccountOwner);
        given()
                .contentType(APPLICATION_JSON)
                .body(jsonSignIn)
                .post(SIGN_IN)
                .then()
                .log().body()
                .statusCode(HttpStatus.FORBIDDEN.value());
    }

    @Test
    @Order(240)
    void successSignInByUserAccountOwnerWithNewEmail() {
        itHelper.getAccountOwner().setEmail(newEmailAccountOwner);
        successSignInAccountOwner();
    }


    private static Stream<Arguments> getAllUsers() {
        return itHelper.getStreamUsers(itHelper.getRoleTestHelper().getPredefinedValidEntityList(), null);
    }

    private static Stream<Arguments> getTokensOfAllUsersNotAccountOwner() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(ITHelper.ADMIN,
                        ITHelper.AUTHOR,
                        ITHelper.EXECUTOR,
                        ITHelper.OBSERVER
                )
        );
        return itHelper.getStreamTokensOfUsers(roles, null);
    }

    private void successSignInAccountOwner() {
        var userAccountOwner = itHelper.getAccountOwner();
        var jsonSignIn = userTestHelper.convertUserToAuthenticationRequestDto(userAccountOwner);
        Response response = given()
                .contentType(APPLICATION_JSON)
                .body(jsonSignIn)
                .post(SIGN_IN)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response();
        itHelper.getTokens().put(userAccountOwner.getEmail(), response.path("token"));
    }
}
