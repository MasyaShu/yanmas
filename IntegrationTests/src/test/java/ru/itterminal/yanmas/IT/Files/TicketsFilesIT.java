package ru.itterminal.yanmas.IT.Files;

import static io.restassured.RestAssured.given;
import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static ru.itterminal.yanmas.IT.util.ITHelper.ACCOUNT_OWNER;
import static ru.itterminal.yanmas.IT.util.ITHelper.ADMIN;
import static ru.itterminal.yanmas.IT.util.ITHelper.APPLICATION_JSON;
import static ru.itterminal.yanmas.IT.util.ITHelper.AUTHOR;
import static ru.itterminal.yanmas.IT.util.ITHelper.EXECUTOR;
import static ru.itterminal.yanmas.IT.util.ITHelper.FILE;
import static ru.itterminal.yanmas.IT.util.ITHelper.FILE_DATA;
import static ru.itterminal.yanmas.IT.util.ITHelper.MULTIPART;
import static ru.itterminal.yanmas.IT.util.ITHelper.OBSERVER;
import static ru.itterminal.yanmas.IT.util.ITHelper.TICKET;
import static ru.itterminal.yanmas.tickets.service.validator.ticket.logical_validation.FileIsMustHaveUploadedDataBeforeCreateTicketValidator.FILE_IS_NOT_YET_UPLOADED;
import static ru.itterminal.yanmas.tickets.service.validator.ticket.logical_validation.FileMustHaveEmptyLinkToEntityBeforeCreateTicketValidator.FILE_ALREADY_HAS_A_LINK_TO_ANOTHER_ENTITY;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;

import io.restassured.RestAssured;
import io.restassured.http.ContentType;
import ru.itterminal.yanmas.IT.util.ITHelper;
import ru.itterminal.yanmas.IT.util.ITTestConfig;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.repository.UserRepository;
import ru.itterminal.yanmas.commons.exception.error.ApiError;
import ru.itterminal.yanmas.files.model.dto.FileDto;
import ru.itterminal.yanmas.files.repository.FileRepository;
import ru.itterminal.yanmas.security.jwt.JwtProvider;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.model.dto.TicketDtoResponse;
import ru.itterminal.yanmas.tickets.model.test.TicketTestHelper;
import ru.itterminal.yanmas.tickets.repository.TicketRepository;

@DataJpaTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = {ITTestConfig.class, JwtProvider.class, UserRepository.class, FileRepository.class,
        TicketRepository.class})
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@TestPropertySource(properties = {"jwt.token.secret=ksedtob", "jwt.token.expired=8640000", "jwt.token.prefix=Bearer"})
class TicketsFilesIT {

    @Autowired
    private FileRepository fileRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private TicketRepository ticketRepository;

    private static final ITHelper itHelper = new ITHelper();
    private final Map<User, FileDto> initialFiles = new HashMap<>();
    @SuppressWarnings("MismatchedQueryAndUpdateOfCollection")
    private final Map<User, byte[]> initialFilesData = new HashMap<>();
    private final TicketTestHelper ticketTestHelper = new TicketTestHelper();
    private final Map<User, TicketDtoResponse> createdTickets = new HashMap<>();

    byte[] biggestFileData = new byte[27000000];

    @BeforeAll
    void beforeAll() {
        RestAssured.useRelaxedHTTPSValidation();
        RestAssured.baseURI = "https://localhost:8080";
        RestAssured.basePath = "/api/v1/";
        itHelper.createAndVerifyAccount(userRepository);
        itHelper.createInitialInnerAndOuterGroups(1, 2);
        itHelper.createInitialUsers();
        itHelper.createInitialTicketType();
        itHelper.createInitialTicketSettings();
        itHelper.createInitialGroupTicketTypes();
        itHelper.createInitialTickets();
        new Random().nextBytes(biggestFileData);
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllInitialUsers")
    @Order(10)
    void SuccessCreateFileByAllInitialUsers(String userKey, User currentUser) {
        var expectedFileDto = FileDto.builder()
                .fileName(itHelper.getFaker().file().fileName())
                .build();
        var actualFileDto = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(expectedFileDto)
                .post(FILE)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(FileDto.class);
        assertEquals(expectedFileDto.getFileName(), actualFileDto.getFileName());
        initialFiles.put(currentUser, actualFileDto);
    }

    @Test
    @Order(20)
    void LogicalExceptionWhenPutDataBecauseSizeOfFileMustNotBeMoreThan25MBByAllInitialUsers() {
        var accountOwner = itHelper.getAccountOwner();
        var fileCreatedByCurrentUser = initialFiles.get(accountOwner);
        var apiError = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(accountOwner.getEmail())
                )
                .contentType(MULTIPART)
                .multiPart(
                        "file",
                        fileCreatedByCurrentUser.getFileName(),
                        biggestFileData,
                        "text/plain"
                )
                .pathParam("fileId", fileCreatedByCurrentUser.getId())
                .post(FILE_DATA)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value())
                .extract().response().as(ApiError.class);
        assertEquals("Maximum upload size exceeded", apiError.getTitle());
    }

    @Test
    @Order(30)
    void LogicalExceptionBecauseFileMustBeUploadedBeforeGetData() {
        var accountOwner = itHelper.getAccountOwner();
        var fileCreatedByAccountOwner = initialFiles.get(accountOwner);
        var apiError = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(accountOwner.getEmail())
                )
                .contentType(ContentType.BINARY)
                .pathParam("fileId", fileCreatedByAccountOwner.getId())
                .get(FILE_DATA)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value())
                .extract().response().as(ApiError.class);
        assertEquals("File wasn't upload", apiError.getErrors().get("File").get(0).getMessage());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllInitialUsers")
    @Order(40)
    void SuccessPutFileDataByAllInitialUsers(String userKey, User currentUser) {
        byte[] fileData = new byte[10];
        new Random().nextBytes(fileData);
        var fileCreatedByCurrentUser = initialFiles.get(currentUser);
        var resultAfterPutData = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(MULTIPART)
                .multiPart(
                        "file",
                        fileCreatedByCurrentUser.getFileName(),
                        fileData,
                        "text/plain"
                )
                .pathParam("fileId", fileCreatedByCurrentUser.getId())
                .post(FILE_DATA)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(Boolean.class);
        assertTrue(resultAfterPutData);
        initialFilesData.put(currentUser, fileData);
    }

    @Test
    @Order(50)
    void LogicalExceptionWhenPutFileDataBecauseFileDataWasAlreadyUploaded() {
        byte[] fileData = new byte[10];
        new Random().nextBytes(fileData);
        var accountOwner = itHelper.getAccountOwner();
        var fileCreatedByAccountOwner = initialFiles.get(accountOwner);
        var apiError = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(accountOwner.getEmail())
                )
                .contentType(MULTIPART)
                .multiPart(
                        "file",
                        fileCreatedByAccountOwner.getFileName(),
                        fileData,
                        "text/plain"
                )
                .pathParam("fileId", fileCreatedByAccountOwner.getId())
                .post(FILE_DATA)
                .then()
                .log().body()
                .statusCode(HttpStatus.CONFLICT.value())
                .extract().response().as(ApiError.class);

        assertEquals("File was already uploaded", apiError.getErrors().get("File").get(0).getMessage());
    }

    @SuppressWarnings("unused")
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllInitialUsers")
    @Order(60)
    void SuccessGetDataByAllInitialUsers(String userKey, User currentUser) {
        var fileCreatedByCurrentUser = initialFiles.get(currentUser);
        var actualFileData = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(ContentType.BINARY)
                .pathParam("fileId", fileCreatedByCurrentUser.getId())
                .get(FILE_DATA)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().asByteArray();
        var expectedFileData = initialFilesData.get(currentUser);
        assertArrayEquals(expectedFileData, actualFileData);
    }

    @Test
    @Order(70)
    void BadRequestBeforeCreateFileBecauseNameOfFileMustLessThan256Characters() {
        var expectedFileDto = FileDto.builder()
                .fileName(itHelper.getFaker().lorem().characters(257))
                .build();
        given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(expectedFileDto)
                .post(FILE)
                .then()
                .log().body()
                .body(containsString("size must be between 0 and 256"))
                .statusCode(HttpStatus.BAD_REQUEST.value());
    }

    @Test
    @Order(80)
    void BadRequestBeforeCreateFileBecauseNameOfFileMustNotBeBlank() {
        var expectedFileDto = FileDto.builder()
                .fileName("  ")
                .build();
        given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(itHelper.getAccountOwner().getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(expectedFileDto)
                .post(FILE)
                .then()
                .log().body()
                .body(containsString("must not be blank"))
                .statusCode(HttpStatus.BAD_REQUEST.value());
    }

    @SuppressWarnings({"unused", "OptionalGetWithoutIsPresent"})
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersWithRolesAdminExecutorAuthor")
    @Order(90)
    void LogicalErrorBeforeCreateTicketWithFileBecauseFileWasCreatedUserWhichIsNotEqualCurrentUser(String userKey, User currentUser) {
        var fileCreatedByAccountOwner = initialFiles.get(itHelper.getAccountOwner());
        var newTicket = Ticket.builder()
                .author(currentUser)
                .subject(TICKET)
                .build();
        var newTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(newTicket, true);
        newTicketDtoRequest.setFiles(List.of(fileCreatedByAccountOwner.getId()));
        var apiError = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(newTicketDtoRequest)
                .post(TICKET)
                .then()
                .log().body()
                .body(containsString("Access denied, because file was created by another user, you cannot use it for create this ticket"))
                .statusCode(HttpStatus.FORBIDDEN.value())
                .extract().response().as(ApiError.class);
    }

    @SuppressWarnings({"unused", "OptionalGetWithoutIsPresent"})
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersWithRolesAccountOwnerAdminExecutorAuthor")
    @Order(100)
    void SuccessCreateTicketWithFileAndAfterCheckThatFileHasLinkToCreatedTicket(String userKey, User currentUser) {
        var fileCreatedByCurrentUser = initialFiles.get(currentUser);
        var newTicket = Ticket.builder()
                .author(currentUser)
                .subject(TICKET)
                .build();
        var newTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(newTicket, true);
        newTicketDtoRequest.setFiles(List.of(fileCreatedByCurrentUser.getId()));
        var createdTicket = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(newTicketDtoRequest)
                .post(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(TicketDtoResponse.class);
        var updatedFileAfterCreateTicket = fileRepository.findByIdAndAccountId(
                fileCreatedByCurrentUser.getId(),
                currentUser.getAccount().getId()
        ).get();
        assertEquals(updatedFileAfterCreateTicket.getEntityId(), createdTicket.getId());
        createdTickets.put(currentUser, createdTicket);
    }

    @SuppressWarnings({"unused", "OptionalGetWithoutIsPresent"})
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersWithRolesAccountOwnerAdminExecutorAuthor")
    @Order(110)
    void SuccessUpdateTicketWithFileAndAfterCheckThatFileHasNotChanged(String userKey, User currentUser) {
        var ticketDtoResponseCreatedByCurrentUser = createdTickets.get(currentUser);
        var fileCreatedByCurrentUser = initialFiles.get(currentUser);
        var fileBeforeUpdateTicket = fileRepository.findByIdAndAccountId(
                fileCreatedByCurrentUser.getId(),
                currentUser.getAccount().getId()
        ).get();
        var ticketForUpdate = ticketRepository.findByIdAndAccountId(
                ticketDtoResponseCreatedByCurrentUser.getId(),
                currentUser.getAccount().getId()
        ).get();
        ticketForUpdate.setSubject(itHelper.getFaker().funnyName().name());
        var ticketDtoRequestForUpdate = ticketTestHelper.convertEntityToDtoRequest(ticketForUpdate, false);
        var updatedTicketDtoResponse = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(ticketDtoRequestForUpdate)
                .put(TICKET)
                .then()
                .log().body()
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(TicketDtoResponse.class);
        assertThat(ticketDtoResponseCreatedByCurrentUser).usingRecursiveComparison()
                .ignoringFields("version", "subject", "displayName").isEqualTo(updatedTicketDtoResponse);
        var fileAfterUpdateTicket = fileRepository.findByIdAndAccountId(
                fileCreatedByCurrentUser.getId(),
                currentUser.getAccount().getId()
        ).get();
        assertThat(fileBeforeUpdateTicket).usingRecursiveComparison().isEqualTo(fileAfterUpdateTicket);
    }

    @SuppressWarnings({"unused", "OptionalGetWithoutIsPresent"})
    @ParameterizedTest(name = "{index} User: {0}")
    @MethodSource("getStreamAllUsersWithRolesAccountOwnerAdminExecutorAuthor")
    @Order(120)
    void LogicalErrorBeforeCreateTicketWithFileBecauseFileHasLinkToAnotherTicket(String userKey, User currentUser) {
        var fileCreatedByCurrentUser = initialFiles.get(currentUser);
        var newTicket = Ticket.builder()
                .author(currentUser)
                .subject(TICKET)
                .build();
        var newTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(newTicket, true);
        newTicketDtoRequest.setFiles(List.of(fileCreatedByCurrentUser.getId()));
        var apiError = given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(currentUser.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(newTicketDtoRequest)
                .post(TICKET)
                .then()
                .log().body()
                .body(containsString(FILE_ALREADY_HAS_A_LINK_TO_ANOTHER_ENTITY))
                .statusCode(HttpStatus.CONFLICT.value())
                .extract().response().as(ApiError.class);
    }

    @Test
    @Order(130)
    void LogicalErrorBeforeCreateTicketWithFileBecauseFileWasNotUploaded() {
        var accountOwner = itHelper.getAccountOwner();
        var fileDtoForCreate = FileDto.builder()
                .fileName(itHelper.getFaker().file().fileName())
                .build();
        var createdFileDto = given().
                when().
                headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(accountOwner.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(fileDtoForCreate)
                .post(FILE)
                .then()
                .log().body()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(FileDto.class);
        var newTicket = Ticket.builder()
                .author(accountOwner)
                .subject(TICKET)
                .build();
        var newTicketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(newTicket, true);
        newTicketDtoRequest.setFiles(List.of(createdFileDto.getId()));
        given().
                when()
                .headers(
                        "Authorization",
                        "Bearer " + itHelper.getTokens().get(accountOwner.getEmail())
                )
                .contentType(APPLICATION_JSON)
                .body(newTicketDtoRequest)
                .post(TICKET)
                .then()
                .log().body()
                .body(containsString(FILE_IS_NOT_YET_UPLOADED))
                .statusCode(HttpStatus.CONFLICT.value());
    }

    @SuppressWarnings("unused")
    private static Stream<Arguments> getStreamAllInitialUsers() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(ACCOUNT_OWNER, ADMIN, EXECUTOR, AUTHOR, OBSERVER
                )
        );
        return itHelper.getStreamUsers(roles, null);
    }

    @SuppressWarnings("unused")
    private static Stream<Arguments> getStreamAllUsersWithRolesAccountOwnerAdminExecutorAuthor() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(AUTHOR, EXECUTOR, ADMIN, ACCOUNT_OWNER)
        );
        return itHelper.getStreamUsers(roles, null);
    }

    @SuppressWarnings("unused")
    private static Stream<Arguments> getStreamAllUsersWithRolesAdminExecutorAuthor() {
        var roles = itHelper.getRoleTestHelper().getRolesByNames(
                List.of(AUTHOR, EXECUTOR, ADMIN)
        );
        return itHelper.getStreamUsers(roles, null);
    }

}
