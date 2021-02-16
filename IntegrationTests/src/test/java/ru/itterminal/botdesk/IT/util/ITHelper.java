package ru.itterminal.botdesk.IT.util;

import static io.restassured.RestAssured.given;
import static io.restassured.RestAssured.when;
import static org.hamcrest.Matchers.equalTo;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Stream;

import org.junit.jupiter.params.provider.Arguments;
import org.modelmapper.ModelMapper;
import org.springframework.http.HttpStatus;

import com.fasterxml.jackson.databind.ObjectMapper;

import groovy.util.MapEntry;
import io.restassured.response.Response;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.test.AccountTestHelper;
import ru.itterminal.botdesk.aau.model.test.GroupTestHelper;
import ru.itterminal.botdesk.aau.model.test.RoleTestHelper;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.aau.repository.UserRepository;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.TicketType;

@Getter
@Setter
@NoArgsConstructor
public class ITHelper {

    private Account account;
    private User accountOwner;
    private Map<String, TicketStatus> ticketStatuses = new HashMap<>();
    private Map<String, TicketType> ticketTypes = new HashMap<>();
    private Map<String, Group> innerGroup = new HashMap<>();
    private Map<String, Group> outerGroup = new HashMap<>();
    private Map<String, User> adminInnerGroup = new HashMap<>();
    private Map<String, User> adminOuterGroup = new HashMap<>();
    private Map<String, User> executorInnerGroup = new HashMap<>();
    private Map<String, User> executorOuterGroup = new HashMap<>();
    private Map<String, User> authorInnerGroup = new HashMap<>();
    private Map<String, User> authorOuterGroup = new HashMap<>();
    private Map<String, User> observerInnerGroup = new HashMap<>();
    private Map<String, User> observerOuterGroup = new HashMap<>();
    private Map<String, String> tokens = new HashMap<>();

    private final UserTestHelper userTestHelper = new UserTestHelper();
    private final AccountTestHelper accountTestHelper = new AccountTestHelper();
    private final RoleTestHelper roleTestHelper = new RoleTestHelper();
    private final GroupTestHelper groupTestHelper = new GroupTestHelper();
    protected final ModelMapper modelMapper = new ModelMapper();
    private final ObjectMapper objectMapper = new ObjectMapper();

    public static final String CREATE_ACCOUNT = "create-account";
    public static final String APPLICATION_JSON = "application/json";
    public static final String REQUEST_PASSWORD_RESET = "auth/request-password-reset";
    public static final String NOT_EXIST_EMAIL = "notExisEmail@gmail.com";
    public static final String NOT_FOUND_USER_BY_EMAIL = "Not found user by email: ";
    public static final String SIGN_IN = "auth/signin";
    public static final String EMAIL_VERIFY = "auth/email-verify";
    public static final String USER = "user";
    public static final String ROLE = "role";
    public static final String INVALID_USERNAME_OR_PASSWORD = "invalid username or password";
    public static final String AUTHENTICATION_FAILED = "authentication failed";
    public static final String STATUS = "status";
    public static final String TITLE = "title";
    public static final String DETAIL = "detail";
    public static final String DELETED = "deleted";
    public static final String VERSION = "version";
    public static final String NAME = "name";
    public static final String ACCOUNT = "account";
    public static final String DISPLAY_NAME = "displayName";
    public static final String OUT_ID = "outId";
    public static final String ID = "id";
    public static final String VALIDATION_FAILED = "Validation failed";
    public static final String INPUT_VALIDATION_FAILED = "Input Validation Failed";
    public static final String TOKEN = "token";
    public static final String TYPE = "type";
    public static final String ENTITY_NOT_EXIST_EXCEPTION =
            "ru.itterminal.botdesk.commons.exception.EntityNotExistException";
    public static final String INNER_GROUP_1 = "innerGroup_1";
    public static final String COMMENT = "comment";
    public static final String IS_INNER = "isInner";
    public static final String IS_DEPRECATED = "isDeprecated";
    public static final String OUTER_GROUP = "outerGroup_";
    public static final String GROUP = "group";
    public static final String INNER_GROUP = "innerGroup_";
    public static final String IS_PREDEFINED_FOR_NEW_TICKET = "isPredefinedForNewTicket";
    public static final String IS_STARTED_PREDEFINED = "isStartedPredefined";
    public static final String IS_FINISHED_PREDEFINED = "isFinishedPredefined";
    public static final String IS_REOPENED_PREDEFINED = "isReopenedPredefined";
    public static final String IS_CANCELED_PREDEFINED = "isCanceledPredefined";
    public static final String ADMIN_INNER_GROUP = "adminInnerGroup_";
    public static final String EXECUTOR_INNER_GROUP = "executorInnerGroup_";
    public static final String AUTHOR_INNER_GROUP = "authorInnerGroup_";
    public static final String OBSERVER_INNER_GROUP = "observerInnerGroup_";
    public static final String ADMIN_OUTER_GROUP = "adminOuterGroup_";
    public static final String EXECUTOR_OUTER_GROUP = "executorOuterGroup_";
    public static final String AUTHOR_OUTER_GROUP = "authorOuterGroup_";
    public static final String OBSERVER_OUTER_GROUP = "observerOuterGroup_";
    public static final String ADMIN = "ADMIN";
    public static final String EXECUTOR = "EXECUTOR";
    public static final String AUTHOR = "AUTHOR";
    public static final String OBSERVER = "OBSERVER";

    public void createAccount() {
        var anonymousUser = userTestHelper.getRandomValidEntity();
        var accountCreateDto = accountTestHelper.convertUserToAccountCreateDto(anonymousUser);
        var response = given().
                when().
                contentType(APPLICATION_JSON).
                body(accountCreateDto).
                post(CREATE_ACCOUNT).
                then()
                .body(NAME, equalTo(anonymousUser.getAccount().getName()))
                .body(DELETED, equalTo(false))
                .body(VERSION, equalTo(0))
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract()
                .response();
        account = response.as(Account.class);
        accountOwner.setEmail(anonymousUser.getEmail());
        accountOwner.setName(anonymousUser.getName());
        accountOwner.setPassword(anonymousUser.getPassword());
        accountOwner.setAccount(account);
        accountOwner.setRole(roleTestHelper.getRoleByName(Roles.ACCOUNT_OWNER.toString()));
    }

    public void verifyEmailOfAccountOwner(UserRepository userRepository) {
        var accountOwnerFromDatabase = userRepository.getByEmail(accountOwner.getEmail()).get();
        given().
                when()
                .param(TOKEN, accountOwnerFromDatabase.getEmailVerificationToken())
                .get(EMAIL_VERIFY)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value());
        accountOwner.setEmailVerificationStatus(true);
        accountOwner.setEmailVerificationToken(null);
        accountOwner.setGroup(accountOwnerFromDatabase.getGroup());
    }

    public void setNestedFieldsInAccountOwner() {
        var groupAccountOwner = getGroupByIdForActivateAccount(accountOwner.getGroup().getId());
        groupAccountOwner.setAccount(account);
        var roleAccountOwner = roleTestHelper.getRoleByName(Roles.ACCOUNT_OWNER.toString());
        accountOwner.setGroup(groupAccountOwner);
        accountOwner.setAccount(account);
        accountOwner.setRole(roleAccountOwner);
        innerGroup.put(INNER_GROUP_1, groupAccountOwner);
    }

    public void signInUser(User user) {
        var authenticationRequestDto = userTestHelper.convertUserToAuthenticationRequestDto(user);
        Response response = given()
                .contentType(APPLICATION_JSON)
                .body(authenticationRequestDto)
                .post(SIGN_IN)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response();
        tokens.put(user.getEmail(), response.path("token"));
    }

    public void createAndVerifyAccount(UserRepository userRepository) {
        createAccount();
        verifyEmailOfAccountOwner(userRepository);
        signInUser(accountOwner);
        setNestedFieldsInAccountOwner();
    }

    private void createInitialGroups(int countGroup, boolean isInnerGroup) {
        String prefix = OUTER_GROUP;
        if (isInnerGroup) {
            prefix = INNER_GROUP;
        }
        for (int i = 1; i <= countGroup; i++) {
            var group = groupTestHelper.getRandomValidEntity();
            group.setAccount(account);
            group.setIsInner(isInnerGroup);
            var groupDto = groupTestHelper.convertEntityToDtoRequest(group, true);
            Response response = given().
                    when()
                    .headers(
                            "Authorization",
                            "Bearer " + tokens.get(accountOwner.getEmail())
                    )
                    .contentType(APPLICATION_JSON).
                            body(groupDto).
                            post(GROUP)
                    .then().log().body()
                    .extract().response();
            var createdGroup = response.as(Group.class);
            createdGroup.setAccount(account);
            if (isInnerGroup) {
                var suffixKey = innerGroup.size() + 1;
                var keyGroup = prefix + suffixKey;
                innerGroup.put(keyGroup, createdGroup);
            } else {
                var suffixKey = outerGroup.size() + 1;
                var keyGroup = prefix + suffixKey;
                outerGroup.put(keyGroup, createdGroup);
            }
        }
    }

    public void createInitialInnerAndOuterGroups(int countInnerGroup, int countOuterGroup) {
        createInitialGroups(countOuterGroup, false);
        createInitialGroups(countInnerGroup, true);
    }

    public void createUsersForEachRoleInGroup(Group group, List<Role> roles) {
        for (Role role : roles) {
            var newUser = userTestHelper.getRandomValidEntity();
            var userDtoRequest = userTestHelper.convertUserToUserDtoRequest(newUser, true);
            userDtoRequest.setRole(role.getId());
            userDtoRequest.setGroup(group.getId());

            User createdUser = given()
                    .headers(
                            "Authorization",
                            "Bearer " + tokens.get(accountOwner.getEmail())
                    )
                    .contentType(APPLICATION_JSON)
                    .body(userDtoRequest)
                    .post(USER)
                    .then()
                    .log().body()
                    .statusCode(HttpStatus.CREATED.value())
                    .extract()
                    .response().as(User.class);
            createdUser.setAccount(account);
            createdUser.setRole(role);
            createdUser.setGroup(group);
            createdUser.setPassword(newUser.getPassword());
            var suffixKey = 1;
            if (group.getIsInner()) {
                switch (role.getName()) {
                    case ADMIN -> {
                        suffixKey = adminInnerGroup.size() + 1;
                        adminInnerGroup.put(ADMIN_INNER_GROUP + suffixKey, createdUser);
                    }
                    case EXECUTOR -> {
                        suffixKey = executorInnerGroup.size() + 1;
                        executorInnerGroup.put(EXECUTOR_INNER_GROUP + suffixKey, createdUser);
                    }
                    case AUTHOR -> {
                        suffixKey = authorInnerGroup.size() + 1;
                        authorInnerGroup.put(AUTHOR_INNER_GROUP + suffixKey, createdUser);
                    }
                    case OBSERVER -> {
                        suffixKey = observerInnerGroup.size() + 1;
                        observerInnerGroup.put(OBSERVER_INNER_GROUP + suffixKey, createdUser);
                    }
                    default -> throw new IllegalStateException("Unexpected value: " + role.getName());
                }
            } else {
                switch (role.getName()) {
                    case ADMIN -> {
                        suffixKey = adminOuterGroup.size() + 1;
                        adminOuterGroup.put(ADMIN_OUTER_GROUP + suffixKey, createdUser);
                    }
                    case EXECUTOR -> {
                        suffixKey = executorOuterGroup.size() + 1;
                        executorOuterGroup.put(EXECUTOR_OUTER_GROUP + suffixKey, createdUser);
                    }
                    case AUTHOR -> {
                        suffixKey = authorOuterGroup.size() + 1;
                        authorOuterGroup.put(AUTHOR_OUTER_GROUP + suffixKey, createdUser);
                    }
                    case OBSERVER -> {
                        suffixKey = observerOuterGroup.size() + 1;
                        observerOuterGroup.put(OBSERVER_OUTER_GROUP + suffixKey, createdUser);
                    }
                    default -> throw new IllegalStateException("Unexpected value: " + role.getName());
                }
            }

            var jsonSignIn = userTestHelper.convertUserToAuthenticationRequestDto(createdUser);
            Response response = given()
                    .contentType(APPLICATION_JSON)
                    .body(jsonSignIn)
                    .post(SIGN_IN)
                    .then()
                    .log().body()
                    .statusCode(HttpStatus.OK.value())
                    .extract()
                    .response();
            tokens.put(createdUser.getEmail(), response.path("token"));
        }
    }

    private Group getGroupByIdForActivateAccount(UUID groupId) {
        return given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + tokens.get(accountOwner.getEmail())
                )
                .pathParam(ID, groupId)
                .get(GROUP + "/{id}")
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(Group.class);
    }

    public Stream<Arguments> getTokensOfUsersWhoDoNotHaveAccessForUpdateAccount() {
        Map<String, User> allUsers = new HashMap<>();
        allUsers.putAll(adminInnerGroup);
        allUsers.putAll(adminOuterGroup);
        allUsers.putAll(executorInnerGroup);
        allUsers.putAll(executorOuterGroup);
        allUsers.putAll(authorInnerGroup);
        allUsers.putAll(authorOuterGroup);
        allUsers.putAll(observerInnerGroup);
        allUsers.putAll(observerOuterGroup);
        return allUsers.entrySet().stream()
                .map(entry -> Arguments.of(entry.getKey(), tokens.get(entry.getValue().getEmail())));
    }

}

