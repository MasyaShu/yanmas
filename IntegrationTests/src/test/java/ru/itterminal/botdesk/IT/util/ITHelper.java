package ru.itterminal.botdesk.IT.util;

import static io.restassured.RestAssured.given;
import static io.restassured.path.json.JsonPath.from;
import static org.hamcrest.Matchers.equalTo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.params.provider.Arguments;
import org.modelmapper.ModelMapper;
import org.springframework.http.HttpStatus;

import com.fasterxml.jackson.databind.ObjectMapper;

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
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.dto.TicketSettingDtoResponse;
import ru.itterminal.botdesk.tickets.model.test.TicketSettingTestHelper;

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
    private Map<String, TicketSetting> ticketSettings = new HashMap<>();

    private final UserTestHelper userTestHelper = new UserTestHelper();
    private final AccountTestHelper accountTestHelper = new AccountTestHelper();
    private final RoleTestHelper roleTestHelper = new RoleTestHelper();
    private final GroupTestHelper groupTestHelper = new GroupTestHelper();
    private final TicketSettingTestHelper ticketSettingTestHelper = new TicketSettingTestHelper();
    protected final ModelMapper modelMapper = new ModelMapper();
    private final ObjectMapper objectMapper = new ObjectMapper();

    private final  List<Role> allRolesWithoutAccountOwner = roleTestHelper.getRolesByNames(
            List.of(
                    ITHelper.ADMIN,
                    ITHelper.AUTHOR,
                    ITHelper.EXECUTOR,
                    ITHelper.OBSERVER
            )
    );

    public static final String TICKET_SETTING = "ticket-setting";
    public static final String CREATE_ACCOUNT = "create-account";
    public static final String APPLICATION_JSON = "application/json";
    public static final String REQUEST_PASSWORD_RESET = "auth/request-password-reset";
    public static final String NOT_EXIST_EMAIL = "notExisEmail@gmail.com";
    public static final String NOT_FOUND_USER_BY_EMAIL = "Not found user by email: ";
    public static final String SIGN_IN = "auth/signin";
    public static final String EMAIL_VERIFY = "auth/email-verify";
    public static final String USER = "user";
    public static final String USER_BY_ID = "user/{id}";
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
    public static final String ACCOUNT_OWNER_INNER_GROUP = "accountOwnerInnerGroup";
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
    public static final String ACCOUNT_OWNER = "ACCOUNT_OWNER";
    public static final String EMPTY_BODY = "{}";
    public static final String GROUP_BY_ID = "group/{id}";
    public static final String SIZE = "size";
    public static final String TOTAL_ELEMENTS = "totalElements";
    public static final String CONTENT = "content";
    public static final String[] IGNORE_FIELDS_FOR_COMPARE_USERS = {"account", "group", "role", "password",
            "emailVerificationStatus", "passwordResetToken", "emailVerificationToken"};
    public static final String TICKET_STATUS = "ticket-status";
    public static final String TICKET_STATUS_BY_ID = "ticket-status/{id}";
    public static final String ERRORS = "errors";
    public static final String TICKET_TYPE = "ticket-type";
    public static final String TICKET_TYPE_BY_ID = "ticket-type/{id}";

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
        accountOwner = new User();
        accountOwner.setEmail(anonymousUser.getEmail());
        accountOwner.setName(anonymousUser.getName());
        accountOwner.setPassword(anonymousUser.getPassword());
        accountOwner.setAccount(account);
        accountOwner.setRole(roleTestHelper.getRoleByName(Roles.ACCOUNT_OWNER.toString()));
    }

    @SuppressWarnings("OptionalGetWithoutIsPresent")
    public void verifyEmailOfAccountOwner(UserRepository userRepository) {
        var accountOwnerFromDatabase = userRepository.getByEmail(accountOwner.getEmail()).get();
        given().
                when()
                .param(TOKEN, accountOwnerFromDatabase.getEmailVerificationToken())
                .get(EMAIL_VERIFY)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value());
        accountOwner.setId(accountOwnerFromDatabase.getId());
        accountOwner.setEmailVerificationStatus(true);
        accountOwner.setEmailVerificationToken(null);
        accountOwner.setGroup(accountOwnerFromDatabase.getGroup());
    }

    @SuppressWarnings("deprecation")
    public void setNestedFieldsInAccountOwner() {
        var groupAccountOwner = getGroupByIdForActivateAccount(accountOwner.getGroup().getId());
        groupAccountOwner.setAccount(account);
        var roleAccountOwner = roleTestHelper.getRoleByName(Roles.ACCOUNT_OWNER.toString());
        accountOwner.generateDisplayName();
        accountOwner.setVersion(1);
        accountOwner.setDeleted(false);
        accountOwner.setIsArchived(false);
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
        updateUser(accountOwner);
        updateTicketStatuses();
        updateTicketTypes();
    }

    private void updateTicketTypes() {
        var response =
                given().
                        when()
                        .headers(
                                "Authorization",
                                "Bearer " + tokens.get(accountOwner.getEmail())
                        )
                        .contentType(APPLICATION_JSON)
                        .body(EMPTY_BODY)
                        .get(TICKET_TYPE)
                        .then()
                        .log().body()
                        .statusCode(HttpStatus.OK.value())
                        .extract().response().asString();
        List<TicketType> ticketTypesList = from(response).getList(CONTENT, TicketType.class);
        for (TicketType ticketType : ticketTypesList) {
            ticketType.setAccount(account);
            if (ticketType.getIsPredefinedForNewTicket()) {
                ticketTypes.put(IS_PREDEFINED_FOR_NEW_TICKET, ticketType);
            }
        }
    }

    private void updateTicketStatuses() {
        var response = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + tokens.get(accountOwner.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(EMPTY_BODY)
                .get(TICKET_STATUS)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().asString();
        List<TicketStatus> ticketStatusesList = from(response).getList(CONTENT, TicketStatus.class);

        for (TicketStatus ticketStatus : ticketStatusesList) {
            ticketStatus.setAccount(account);
            if (ticketStatus.getIsCanceledPredefined()) {
                ticketStatuses.put(IS_CANCELED_PREDEFINED, ticketStatus);
            }
            if (ticketStatus.getIsFinishedPredefined()) {
                ticketStatuses.put(IS_FINISHED_PREDEFINED, ticketStatus);
            }
            if (ticketStatus.getIsReopenedPredefined()) {
                ticketStatuses.put(IS_REOPENED_PREDEFINED, ticketStatus);
            }
            if (ticketStatus.getIsStartedPredefined()) {
                ticketStatuses.put(IS_STARTED_PREDEFINED, ticketStatus);
            }
        }
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

    public void createInitialUsers() {
        createUsersForEachRoleInGroup(outerGroup.get(OUTER_GROUP + 1), allRolesWithoutAccountOwner);
        createUsersForEachRoleInGroup(innerGroup.get(INNER_GROUP + 1), allRolesWithoutAccountOwner);
        createUsersForEachRoleInGroup(outerGroup.get(OUTER_GROUP + 2), allRolesWithoutAccountOwner);
        createUsersForEachRoleInGroup(innerGroup.get(INNER_GROUP + 2), allRolesWithoutAccountOwner);
    }

    public void createInitialTicketSettings() {
        createInitialTicketSetting(innerGroup.get(INNER_GROUP+"1"));
        createInitialTicketSetting(outerGroup.get(OUTER_GROUP+"1"));
    }

    private void createInitialTicketSetting(Group group) {
        var executors =
                getUser(
                        List.of(roleTestHelper.getRoleByName(Roles.EXECUTOR.toString())),
                        group.getIsInner()
                ).values().stream()
                        .filter(user -> user.getGroup().equals(group))
                        .collect(Collectors.toList());
        var observers =
                getUser(
                        List.of(roleTestHelper.getRoleByName(Roles.OBSERVER.toString())),
                        group.getIsInner()
                ).values().stream()
                        .filter(user -> user.getGroup().equals(group))
                        .collect(Collectors.toList());
        var ticketSetting = TicketSetting.builder()
                .account(account)
                .group(group)
                .executors(executors)
                .observers(observers)
                .build();
        var ticketSettingDtoRequest = ticketSettingTestHelper.convertEntityToDtoRequest(ticketSetting);
        var ticketSettingDtoResponse = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + tokens.get(accountOwner.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketSettingDtoRequest)
                .post(TICKET_SETTING)
                .then()
                .log().body()
                .extract().response().as(TicketSettingDtoResponse.class);
        var key = group.getIsInner()
                ? "InnerGroup_1"
                : "OuterGroup_1";
        var createdTicketSetting = modelMapper.map(ticketSettingDtoResponse, TicketSetting.class);
        ticketSettings.put(key, createdTicketSetting);
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

            var authenticationRequestDto = userTestHelper.convertUserToAuthenticationRequestDto(createdUser);
            Response response = given()
                    .contentType(APPLICATION_JSON)
                    .body(authenticationRequestDto)
                    .post(SIGN_IN)
                    .then()
                    .log().body()
                    .statusCode(HttpStatus.OK.value())
                    .extract()
                    .response();
            tokens.put(createdUser.getEmail(), response.path("token"));
        }
    }

    @SuppressWarnings("unused")
    public User createUserByGivenUserForGivenRoleAndGroupWithSaveInMaps(Group group, Role role, User currentUser) {
        var newUser = userTestHelper.getRandomValidEntity();
        var userDtoRequest = userTestHelper.convertUserToUserDtoRequest(newUser, true);
        userDtoRequest.setRole(role.getId());
        userDtoRequest.setGroup(group.getId());

        User createdUser = given()
                .headers(
                        "Authorization",
                        "Bearer " + tokens.get(currentUser.getEmail())
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
        var authenticationRequestDto = userTestHelper.convertUserToAuthenticationRequestDto(createdUser);
        Response response = given()
                .contentType(APPLICATION_JSON)
                .body(authenticationRequestDto)
                .post(SIGN_IN)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract()
                .response();
        tokens.put(createdUser.getEmail(), response.path("token"));
        return createdUser;
    }

    public User createUserByGivenUserForGivenRoleAndGroupWithoutSaveInMaps(Group group, Role role, User currentUser) {
        var newUser = userTestHelper.getRandomValidEntity();
        var userDtoRequest = userTestHelper.convertUserToUserDtoRequest(newUser, true);
        userDtoRequest.setRole(role.getId());
        userDtoRequest.setGroup(group.getId());

        User createdUser = given()
                .headers(
                        "Authorization",
                        "Bearer " + tokens.get(currentUser.getEmail())
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

        return createdUser;
    }

    @SuppressWarnings("deprecation")
    public void updateUser(User user) {
        var userDtoRequest = userTestHelper.convertUserToUserDtoRequest(user, false);
        var updatedUser = given()
                .headers(
                        "Authorization",
                        "Bearer " + tokens.get(accountOwner.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(userDtoRequest)
                .put(USER)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(User.class);
        user.setVersion(updatedUser.getVersion());
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

    public Stream<Arguments> getStreamTokensOfUsers(List<Role> roles, Boolean isInnerGroup) {
        var allUsers = getUser(roles, isInnerGroup);
        return allUsers.entrySet().stream()
                .map(entry -> Arguments.of(entry.getKey(), tokens.get(entry.getValue().getEmail())));
    }

    public Stream<Arguments> getStreamUsers(List<Role> roles, Boolean isInnerGroup) {
        var allUsers = getUser(roles, isInnerGroup);
        return allUsers.entrySet().stream()
                .map(entry -> Arguments.of(entry.getKey(), entry.getValue()));
    }

    public List<Group> getAllGroup() {
        List<Group> listAllGroup = new ArrayList<>();
        listAllGroup.addAll(outerGroup.values());
        listAllGroup.addAll(innerGroup.values());
        return listAllGroup;
    }

    public void putGroup(Group group) {
        group.setAccount(account);
        if (group.getIsInner()) {
            var suffixKey = innerGroup.size() + 1;
            var nameKey = INNER_GROUP + suffixKey;
            innerGroup.put(nameKey, group);
        } else {
            var suffixKey = outerGroup.size() + 1;
            var nameKey = OUTER_GROUP + suffixKey;
            outerGroup.put(nameKey, group);
        }
    }

    public List<User> getAllUsers() {
        return new ArrayList<>(getUser(roleTestHelper.getPredefinedValidEntityList(), null).values());
    }

    private Map<String, User> getUser(List<Role> roles, Boolean isInnerGroup) {
        Map<String, User> allUsers = new HashMap<>();
        allUsers.putAll(adminInnerGroup);
        allUsers.putAll(adminOuterGroup);
        allUsers.putAll(executorInnerGroup);
        allUsers.putAll(executorOuterGroup);
        allUsers.putAll(authorInnerGroup);
        allUsers.putAll(authorOuterGroup);
        allUsers.putAll(observerInnerGroup);
        allUsers.putAll(observerOuterGroup);
        allUsers.put(ACCOUNT_OWNER_INNER_GROUP, accountOwner);

        return allUsers.entrySet().stream()
                .filter(entry -> {
                            if (entry.getValue().getGroup().getIsInner().equals(isInnerGroup)) {
                                return true;
                            }
                            return isInnerGroup == null;
                        }
                )
                .filter(entry -> roles.contains(entry.getValue().getRole()))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }

}

