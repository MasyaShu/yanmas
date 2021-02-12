package ru.itterminal.botdesk.IT;

import io.restassured.RestAssured;
import io.restassured.response.Response;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.json.JSONObject;
import org.springframework.http.HttpStatus;
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

import static io.restassured.RestAssured.given;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ITHelper {
    private Account account;
    private User accountOwner;
    private Map<String, TicketStatus> ticketStatuses;
    private Map<String, TicketType> ticketTypes;
    private Map<String, Group> innerGroup;
    private Map<String, User> outerGroup;
    private Map<String, User> adminInnerGroup;
    private Map<String, User> adminOuterGroup;
    private Map<String, User> executorInnerGroup;
    private Map<String, User> executorOuterGroup;
    private Map<String, User> authorInnerGroup;
    private Map<String, User> authorOuterGroup;
    private Map<String, User> observerInnerGroup;
    private Map<String, User> observerOuterGroup;
    private Map<String, String> tokens;

    private final UserTestHelper userTestHelper = new UserTestHelper();
    private final AccountTestHelper accountTestHelper= new AccountTestHelper();
    private final RoleTestHelper roleTestHelper = new RoleTestHelper();
    private final GroupTestHelper groupTestHelper = new GroupTestHelper();

    public static final String CREATE_ACCOUNT = "create-account";
    public static final String APPLICATION_JSON = "application/json";

    public void createAccount() {

        RestAssured.useRelaxedHTTPSValidation();
        RestAssured.baseURI = "https://localhost:8080";
        RestAssured.basePath = "/api/v1/";

        User user = userTestHelper.getRandomValidEntity();

        HashMap<String, String> jsonMap = new HashMap<>();
        jsonMap.put("name", user.getAccount().getName());
        jsonMap.put("nameGroupAccountOwner", user.getGroup().getName());
        jsonMap.put("passwordAccountOwner", user.getPassword());
        jsonMap.put("emailAccountOwner", user.getEmail());
        JSONObject jsonRequest = new JSONObject(jsonMap);


        Response response = given().
                when().
                contentType(APPLICATION_JSON).
                body(jsonRequest.toString()).
                post(CREATE_ACCOUNT).
                then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract()
                .response();

        String idAccount = response.path("id");
        String nameAccount = response.path("name");
        Boolean deletedAccount = response.path("deleted");
        Integer versionAccount = response.path("version");
        String displayNameAccount = response.path("displayName");
        String outIdAccount = response.path("outId");

        account = Account.builder()
                .id(UUID.fromString(idAccount))
                .outId(outIdAccount)
                .deleted(deletedAccount)
                .version(versionAccount)
                .displayName(displayNameAccount)
                .name(nameAccount)
                .build();

    }



}
