package ru.itterminal.botdesk.IT.util;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.restassured.response.Response;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.http.HttpStatus;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.dto.*;
import ru.itterminal.botdesk.aau.model.test.AccountTestHelper;
import ru.itterminal.botdesk.aau.model.test.GroupTestHelper;
import ru.itterminal.botdesk.aau.model.test.RoleTestHelper;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.TicketType;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import static io.restassured.RestAssured.given;

@SuppressWarnings("deprecation")
@Slf4j
@Data
@AllArgsConstructor
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
    public static final String USER_GET_BY_FILTER = "user";
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
    public static final String ENTITY_NOT_EXIST_EXCEPTION = "ru.itterminal.botdesk.commons.exception.EntityNotExistException";
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


    public Response getJsonGroupDtoById(UUID groupId, User accountOwner) {
        return given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + tokens.get(accountOwner.getEmail()))
                .pathParam(ID, groupId)
                .get(GROUP + "/{id}")
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response();
    }

    public GroupDto createJsonGroupDto(Group group, boolean isCreate) {
        if (isCreate) {
            group.setId(null);
            group.setDeleted(null);
            group.setVersion(null);
            group.setIsDeprecated(null);
            group.setDisplayName(null);
        }
        return modelMapper.map(group, GroupDto.class);
    }

    public AccountCreateDto createJsonAccountCreateDto(User anonymousUser) {
        return AccountCreateDto.builder()
                .name(anonymousUser.getAccount().getName())
                .emailAccountOwner(anonymousUser.getEmail())
                .passwordAccountOwner(anonymousUser.getPassword())
                .nameGroupAccountOwner(anonymousUser.getEmail())
                .build();
    }

    public AccountDto createJsonAccountDto(Account account) {
        var accountDto = modelMapper.map(account, AccountDto.class);
        accountDto.setDisplayName(null);
        return accountDto;
    }

    public AuthenticationRequestDto createJsonAuthenticationRequestDto(User user) {
        return modelMapper.map(user, AuthenticationRequestDto.class);
    }

    public UserDtoRequest createJsonUserDtoRequest(User createdUser, boolean isCreated) {
        if (isCreated) {
            createdUser.setId(null);
            createdUser.setDeleted(null);
            createdUser.setVersion(null);
            createdUser.setDisplayName(null);
            createdUser.setIsArchived(null);
        }
        return modelMapper.map(createdUser, UserDtoRequest.class);
    }

    public void activateAccount() {
        var groupAccountOwner = getJsonGroupDtoById(accountOwner.getGroup().getId(), accountOwner).as(Group.class);
        groupAccountOwner.setAccount(account);
        var roleAccountOwner = roleTestHelper.getRoleByName(Roles.ACCOUNT_OWNER.toString());
        accountOwner.setGroup(groupAccountOwner);
        accountOwner.setAccount(account);
        accountOwner.setRole(roleAccountOwner);
        innerGroup.put(INNER_GROUP_1, groupAccountOwner);
    }

    private void createInitialOuterGroups(int countGroup) {
        for (int i = 1; i <= countGroup; i++) {
            var group = groupTestHelper.getRandomValidEntity();
            var suffixKey = outerGroup.size() + 1;
            var keyOuterGroup = OUTER_GROUP + suffixKey;
            group.setAccount(account);
            group.setIsInner(false);
            var jsonGroupDto = createJsonGroupDto(group, true);
            Response response = given().
                    when()
                    .headers(
                            "Authorization",
                            "Bearer " + tokens.get(accountOwner.getEmail()))
                    .contentType(APPLICATION_JSON).
                            body(jsonGroupDto).
                            post(GROUP).
                            then()
                    .log().body()
                    .extract().response();
            var createdGroup = response.as(Group.class);
            createdGroup.setAccount(account);
            outerGroup.put(keyOuterGroup, createdGroup);

        }
    }

    private void createInitialInnerGroups(int countGroup) {
        for (int i = 1; i <= countGroup; i++) {
            var group = groupTestHelper.getRandomValidEntity();
            var suffixKey = innerGroup.size() + 1;
            var keyInnerGroup = INNER_GROUP + suffixKey;
            group.setAccount(account);
            group.setIsInner(true);
            var jsonGroupDto = createJsonGroupDto(group, true);
            Response response = given().
                    when()
                    .headers(
                            "Authorization",
                            "Bearer " + tokens.get(accountOwner.getEmail()))
                    .contentType(APPLICATION_JSON).
                            body(jsonGroupDto).
                            post(GROUP).
                            then()
                    .log().body()
                    .extract().response();
            var createdGroup = response.as(Group.class);
            createdGroup.setAccount(account);
            innerGroup.put(keyInnerGroup, createdGroup);
        }
    }

    public void createInitialInnerAndOuterGroups(int countInnerGroup, int countOuterGroup) {
        createInitialInnerGroups(countInnerGroup);
        createInitialOuterGroups(countOuterGroup);
    }
}

