package ru.itterminal.botdesk.IT.util;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.json.JSONObject;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.test.AccountTestHelper;
import ru.itterminal.botdesk.aau.model.test.GroupTestHelper;
import ru.itterminal.botdesk.aau.model.test.RoleTestHelper;
import ru.itterminal.botdesk.aau.model.test.UserTestHelper;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.TicketType;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ITHelper {
    private Account account;
    private User accountOwner;
    private Map<String, TicketStatus> ticketStatuses = new HashMap<>();
    private Map<String, TicketType> ticketTypes = new HashMap<>();
    private Map<String, Group> innerGroup = new HashMap<>();
    private Map<String, User> outerGroup = new HashMap<>();
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
    public static final String VALIDATION_FAILED = "Validation failed";
    public static final String INPUT_VALIDATION_FAILED = "Input Validation Failed";
    public static final String TOKEN = "token";

    @Builder(builderMethodName = "builderAccount")
    public void createAccount(String idAccount, String outIdAccount,
                              Boolean deletedAccount, Integer versionAccount,
                              String displayNameAccount, String nameAccount) {


        account = Account.builder()
                .id(UUID.fromString(idAccount))
                .outId(outIdAccount)
                .deleted(deletedAccount)
                .version(versionAccount)
                .displayName(displayNameAccount)
                .name(nameAccount)
                .build();
    }


    @Builder(builderMethodName = "builderJsonCreateAccount")
    public String createJsonCreateAccount(String name, String nameGroupAccountOwner,
                                           String passwordAccountOwner, String emailAccountOwner) {
        HashMap<String, String> jsonMap = new HashMap<>();
        jsonMap.put("name", name);
        jsonMap.put("nameGroupAccountOwner", nameGroupAccountOwner);
        jsonMap.put("passwordAccountOwner", passwordAccountOwner);
        jsonMap.put("emailAccountOwner", emailAccountOwner);
        JSONObject json = new JSONObject(jsonMap);
        return json.toString();
    }

    @Builder(builderMethodName = "builderJsonSignIn")
    public String createJsonSignIn(String email, String password) {
        HashMap<String, String> jsonMap = new HashMap<>();
        jsonMap.put("email", email);
        jsonMap.put("password", password);
        JSONObject json = new JSONObject(jsonMap);
        return json.toString();
    }

    @Builder(builderMethodName = "test")
    public String testString(String test) {
        return test;
    }
}
