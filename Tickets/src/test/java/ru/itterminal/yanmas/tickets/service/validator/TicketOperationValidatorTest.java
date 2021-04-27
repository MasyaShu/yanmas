package ru.itterminal.yanmas.tickets.service.validator;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.test.RoleTestHelper;
import ru.itterminal.yanmas.commons.exception.LogicalValidationException;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.files.model.File;
import ru.itterminal.yanmas.security.config.TestSecurityConfig;
import ru.itterminal.yanmas.tickets.model.test.TicketTestHelper;
import ru.itterminal.yanmas.tickets.service.impl.SettingsAccessToTicketTypesServiceImpl;
import ru.itterminal.yanmas.tickets.service.impl.TicketServiceImpl;

import java.util.List;
import java.util.UUID;
import java.util.stream.Stream;

import static java.lang.String.format;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl.VALIDATION_FAILED;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createLogicalValidationException;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.yanmas.security.config.TestSecurityConfig.AUTHOR_ACCOUNT_1_IS_INNER_GROUP_ID;
import static ru.itterminal.yanmas.tickets.service.validator.TicketOperationValidator.*;
import static ru.itterminal.yanmas.tickets.service.validator.TicketSettingOperationValidator.ACCESS_DENIED_BECAUSE_CURRENT_USER_HAS_NOT_PERMIT_TO_TICKET_TYPE;

@SpringJUnitConfig(value = {TicketOperationValidator.class})
@Import(TestSecurityConfig.class)
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class TicketOperationValidatorTest {

    @MockBean
    private SettingsAccessToTicketTypesServiceImpl settingsAccessToTicketTypesService;

    @MockBean
    private TicketServiceImpl ticketService;

    @Autowired
    private TicketOperationValidator validator;

    private final TicketTestHelper ticketTestHelper = new TicketTestHelper();
    private final RoleTestHelper roleTestHelper = new RoleTestHelper();

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void logicalValidationBeforeCreate_shouldGetTrue_whenPassedTicketIsValid() {
        var ticket = ticketTestHelper.getRandomValidEntity();
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(true);
        var actualResult = validator.logicalValidationBeforeCreate(ticket);
        assertTrue(actualResult);
        verify(settingsAccessToTicketTypesService, times(1)).isPermittedTicketType(any(), any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void logicalValidationBeforeCreate_shouldGetLogicalValidationException_whenSubjectDescriptionFilesAreEmpty() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        ticket.setSubject("");
        ticket.setDescription("");
        expectedErrors.put(
                TicketOperationValidator.EMPTY_TICKET,
                singletonList(
                        new ValidationError(
                                TicketOperationValidator.EMPTY_TICKET,
                                TicketOperationValidator.MUST_NOT_CREATE_UPDATE_TICKET_IF_SUBJECT_DESCRIPTION_AND_FILES_ARE_EMPTY
                        )
                )
        );
        LogicalValidationException expectedException =
                new LogicalValidationException(VALIDATION_FAILED, expectedErrors);
        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.logicalValidationBeforeCreate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(TicketOperationValidator.EMPTY_TICKET).get(0),
                actualException.getFieldErrors().get(TicketOperationValidator.EMPTY_TICKET).get(0)
        );
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void logicalValidationBeforeCreate_shouldGetLogicalValidationException_whenWeightOfRoleOfAuthorLessThanAccordingWeightOfRole() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        ticket.getAuthor().setRole(roleTestHelper.getPredefinedValidEntityList().get(4));
        expectedErrors.put(
                TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR,
                singletonList(
                        new ValidationError(
                                TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR,
                                format(
                                        TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR_LESS_THAN_WEIGHT_OF_ROLE_AUTHOR,
                                        ticket.getAuthor().getRole().getWeight(),
                                        Roles.AUTHOR.getWeight()
                                )
                        )
                )
        );
        LogicalValidationException expectedException =
                new LogicalValidationException(VALIDATION_FAILED, expectedErrors);
        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.logicalValidationBeforeCreate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR)
                        .get(0),
                actualException.getFieldErrors().get(TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR).get(0)
        );
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void logicalValidationBeforeCreate_shouldGetLogicalValidationException_whenWeightOfRoleOfObserverLessThanAccordingWeightOfRole() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        var observer = ticket.getAuthor();
        observer.getRole().setWeight(0);
        ticket.setObservers(List.of(observer));
        expectedErrors.put(
                TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS,
                singletonList(
                        new ValidationError(
                                TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS,
                                format(
                                        TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS_LESS_THAN_WEIGHT_OF_ROLE_OBSERVER,
                                        ticket.getObservers().get(0).getRole().getWeight(),
                                        Roles.OBSERVER.getWeight()
                                )
                        )
                )
        );
        LogicalValidationException expectedException =
                new LogicalValidationException(VALIDATION_FAILED, expectedErrors);
        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.logicalValidationBeforeCreate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS)
                        .get(0),
                actualException.getFieldErrors().get(TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS)
                        .get(0)
        );
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void logicalValidationBeforeCreate_shouldGetLogicalValidationException_whenWeightOfRoleOfExecutorLessThanAccordingWeightOfRole() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        var executor = ticket.getAuthor();
        executor.getRole().setWeight(0);
        ticket.setExecutors(List.of(executor));
        expectedErrors.put(
                TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS,
                singletonList(
                        new ValidationError(
                                TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS,
                                format(
                                        TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS_LESS_THAN_WEIGHT_OF_ROLE_EXECUTOR,
                                        ticket.getExecutors().get(0).getRole().getWeight(),
                                        Roles.EXECUTOR.getWeight()
                                )
                        )
                )
        );
        LogicalValidationException expectedException =
                new LogicalValidationException(VALIDATION_FAILED, expectedErrors);
        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.logicalValidationBeforeCreate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS)
                        .get(0),
                actualException.getFieldErrors().get(TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS)
                        .get(0)
        );
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void logicalValidationBeforeCreate_shouldGetLogicalValidationException_whenPassedFileHasLinkToAnotherTicket() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        var invalidFile = File.builder()
                .entityId(UUID.randomUUID())
                .authorId(UUID.fromString(AUTHOR_ACCOUNT_1_IS_INNER_GROUP_ID))
                .isUploaded(true)
                .build();
        ticket.setFiles(List.of(invalidFile));
        expectedErrors.put(
                TicketOperationValidator.FILE_IS_INVALID,
                singletonList(
                        new ValidationError(
                                TicketOperationValidator.FILE_IS_INVALID,
                                TicketOperationValidator.FILE_ALREADY_HAS_A_LINK_TO_ANOTHER_ENTITY
                        )
                )
        );
        LogicalValidationException expectedException =
                new LogicalValidationException(VALIDATION_FAILED, expectedErrors);
        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.logicalValidationBeforeCreate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(TicketOperationValidator.FILE_IS_INVALID).get(0),
                actualException.getFieldErrors().get(TicketOperationValidator.FILE_IS_INVALID).get(0)
        );
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void checkAccessBeforeCreate_shouldGetAccessDeniedException_whenFileWasCreatedByAnotherUser() {
        var ticket = ticketTestHelper.getRandomValidEntity();
        var invalidFile = File.builder()
                .entityId(null)
                .authorId(UUID.randomUUID())
                .isUploaded(true)
                .build();
        ticket.setFiles(List.of(invalidFile));
        var currentUser = ticket.getAuthor().toBuilder().build();
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(true);
        var actualException = assertThrows(AccessDeniedException.class,
                () -> validator.checkAccessBeforeCreate(ticket, currentUser)
        );
        assertEquals(
                ACCESS_DENIED_BECAUSE_FILE_WAS_CREATED_BY_ANOTHER_USER_YOU_CANNOT_USE_IT_FOR_CREATE_THIS_TICKET,
                actualException.getMessage()
        );
        verify(settingsAccessToTicketTypesService, times(1)).isPermittedTicketType(any(), any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void logicalValidationBeforeCreate_shouldGetLogicalValidationException_whenAuthorOfTicketHasNotPermitToTicketType() {
        var ticket = ticketTestHelper.getRandomValidEntity();
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(false);
        var expectedException = createLogicalValidationException(
                INVALID_TICKET,
                INVALID_TICKET_BECAUSE_AUTHOR_HAS_NOT_ACCESS_TO_TICKET_TYPE
        );
        var actualException = assertThrows(
                LogicalValidationException.class,
                () -> validator.logicalValidationBeforeCreate(ticket)
        );
        assertEquals(
                expectedException.getFieldErrors().get(INVALID_TICKET).get(0),
                actualException.getFieldErrors().get(INVALID_TICKET).get(0)
        );
        verify(settingsAccessToTicketTypesService, times(1)).isPermittedTicketType(any(), any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void logicalValidationBeforeCreate_shouldGetLogicalValidationException_whenFileIsNotYetUploaded() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        var invalidFile = File.builder()
                .entityId(null)
                .authorId(UUID.fromString(AUTHOR_ACCOUNT_1_IS_INNER_GROUP_ID))
                .isUploaded(false)
                .build();
        ticket.setFiles(List.of(invalidFile));
        expectedErrors.put(
                TicketOperationValidator.FILE_IS_INVALID,
                singletonList(
                        new ValidationError(
                                TicketOperationValidator.FILE_IS_INVALID,
                                TicketOperationValidator.FILE_IS_NOT_YET_UPLOADED
                        )
                )
        );
        LogicalValidationException expectedException =
                new LogicalValidationException(VALIDATION_FAILED, expectedErrors);
        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.logicalValidationBeforeCreate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(TicketOperationValidator.FILE_IS_INVALID).get(0),
                actualException.getFieldErrors().get(TicketOperationValidator.FILE_IS_INVALID).get(0)
        );
    }


    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void logicalValidationBeforeUpdate_shouldGetLogicalValidationException_whenAuthorOfTicketHasNotPermitToTicketType() {
        var ticket = ticketTestHelper.getRandomValidEntity();
        var ticketBeforeUpdate = ticketTestHelper.getRandomValidEntity();
        when(ticketService.findByIdAndAccountId(any())).thenReturn(ticketBeforeUpdate);
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(false);
        var expectedException = createLogicalValidationException(
                INVALID_TICKET,
                INVALID_TICKET_BECAUSE_AUTHOR_HAS_NOT_ACCESS_TO_TICKET_TYPE
        );
        var actualException = assertThrows(
                LogicalValidationException.class,
                () -> validator.logicalValidationBeforeUpdate(ticket)
        );
        assertEquals(
                expectedException.getFieldErrors().get(INVALID_TICKET).get(0),
                actualException.getFieldErrors().get(INVALID_TICKET).get(0)
        );
        verify(settingsAccessToTicketTypesService, times(1)).isPermittedTicketType(any(), any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void logicalValidationBeforeUpdate_shouldGetLogicalValidationException_whenSubjectDescriptionFilesAreEmpty() {
        var ticketBeforeUpdate = ticketTestHelper.getRandomValidEntity();
        when(ticketService.findByIdAndAccountId(any())).thenReturn(ticketBeforeUpdate);
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        ticket.setSubject("");
        ticket.setDescription("");
        expectedErrors.put(
                TicketOperationValidator.EMPTY_TICKET,
                singletonList(
                        new ValidationError(
                                TicketOperationValidator.EMPTY_TICKET,
                                TicketOperationValidator.MUST_NOT_CREATE_UPDATE_TICKET_IF_SUBJECT_DESCRIPTION_AND_FILES_ARE_EMPTY
                        )
                )
        );
        LogicalValidationException expectedException =
                new LogicalValidationException(VALIDATION_FAILED, expectedErrors);
        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.logicalValidationBeforeUpdate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(TicketOperationValidator.EMPTY_TICKET).get(0),
                actualException.getFieldErrors().get(TicketOperationValidator.EMPTY_TICKET).get(0)
        );
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void logicalValidationBeforeUpdate_shouldGetTrue_whenPassedTicketIsValid() {
        var ticket = ticketTestHelper.getRandomValidEntity();
        var ticketBeforeUpdate = ticketTestHelper.getRandomValidEntity();
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(true);
        when(ticketService.findByIdAndAccountId(any())).thenReturn(ticketBeforeUpdate);
        var actualResult = validator.logicalValidationBeforeUpdate(ticket);
        assertTrue(actualResult);
        verify(settingsAccessToTicketTypesService, times(1)).isPermittedTicketType(any(), any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void logicalValidationBeforeUpdate_shouldGetLogicalValidationException_whenWeightOfRoleOfAuthorLessThanAccordingWeightOfRole() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        var ticketBeforeUpdate = ticketTestHelper.getRandomValidEntity();
        when(ticketService.findByIdAndAccountId(any())).thenReturn(ticketBeforeUpdate);
        ticket.getAuthor().setRole(roleTestHelper.getPredefinedValidEntityList().get(4));
        expectedErrors.put(
                TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR,
                singletonList(
                        new ValidationError(
                                TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR,
                                format(
                                        TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR_LESS_THAN_WEIGHT_OF_ROLE_AUTHOR,
                                        ticket.getAuthor().getRole().getWeight(),
                                        Roles.AUTHOR.getWeight()
                                )
                        )
                )
        );
        LogicalValidationException expectedException =
                new LogicalValidationException(VALIDATION_FAILED, expectedErrors);
        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.logicalValidationBeforeUpdate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR)
                        .get(0),
                actualException.getFieldErrors().get(TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR).get(0)
        );
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void logicalValidationBeforeUpdate_shouldGetLogicalValidationException_whenWeightOfRoleOfObserverLessThanAccordingWeightOfRole() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        var ticketBeforeUpdate = ticketTestHelper.getRandomValidEntity();
        when(ticketService.findByIdAndAccountId(any())).thenReturn(ticketBeforeUpdate);
        var observer = ticket.getAuthor();
        observer.getRole().setWeight(0);
        ticket.setObservers(List.of(observer));
        expectedErrors.put(
                TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS,
                singletonList(
                        new ValidationError(
                                TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS,
                                format(
                                        TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS_LESS_THAN_WEIGHT_OF_ROLE_OBSERVER,
                                        ticket.getObservers().get(0).getRole().getWeight(),
                                        Roles.OBSERVER.getWeight()
                                )
                        )
                )
        );
        LogicalValidationException expectedException =
                new LogicalValidationException(VALIDATION_FAILED, expectedErrors);
        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.logicalValidationBeforeUpdate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS)
                        .get(0),
                actualException.getFieldErrors().get(TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS)
                        .get(0)
        );
        verify(ticketService, times(1)).findByIdAndAccountId(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void logicalValidationBeforeUpdate_shouldGetLogicalValidationException_whenWeightOfRoleOfExecutorLessThanAccordingWeightOfRole() {
        var ticketBeforeUpdate = ticketTestHelper.getRandomValidEntity();
        when(ticketService.findByIdAndAccountId(any())).thenReturn(ticketBeforeUpdate);
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        var executor = ticket.getAuthor();
        executor.getRole().setWeight(0);
        ticket.setExecutors(List.of(executor));
        expectedErrors.put(
                TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS,
                singletonList(
                        new ValidationError(
                                TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS,
                                format(
                                        TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS_LESS_THAN_WEIGHT_OF_ROLE_EXECUTOR,
                                        ticket.getExecutors().get(0).getRole().getWeight(),
                                        Roles.EXECUTOR.getWeight()
                                )
                        )
                )
        );
        LogicalValidationException expectedException =
                new LogicalValidationException(VALIDATION_FAILED, expectedErrors);
        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.logicalValidationBeforeUpdate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS)
                        .get(0),
                actualException.getFieldErrors().get(TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS)
                        .get(0)
        );
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void logicalValidationBeforeUpdate_shouldGetLogicalValidationException_whenNewTicketNotHasAnyChanges() {
        var ticket = ticketTestHelper.getRandomValidEntity();
        when(ticketService.findByIdAndAccountId(any())).thenReturn(ticket);
        var expectedException = createLogicalValidationException(
                NO_CHANGES,
                INVALID_TICKET_BECAUSE_TICKET_DOES_NOT_HAVE_ANY_CHANGES);
        var actualException = assertThrows(
                LogicalValidationException.class,
                () -> validator.logicalValidationBeforeUpdate(ticket)
        );
        assertEquals(
                expectedException.getFieldErrors().get(NO_CHANGES).get(0),
                actualException.getFieldErrors().get(NO_CHANGES).get(0)
        );
    }

    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    @ParameterizedTest(name = "{index} - id of TestDataCrudTicketPermission ")
    @MethodSource("getTestDataForCheckAccessForCreateAndUpdateWhenAccessDeniedException")
    void checkAccessForCreateAndUpdate_ShouldGetAccessDeniedException_whenAccessDenied
            (TestDataCrudTicketPermission testData) {
        var expectedExceptionMessage = testData.exceptionMessage;
        var ticket = ticketTestHelper.getRandomValidEntity();
        var currentUser = ticket.getAuthor().toBuilder().build();
        currentUser.setGroup(ticket.getGroup().toBuilder().build());
        ticket.getAuthor().getGroup().setIsInner(testData.isAuthorOfTicketFromInnerGroup);
        currentUser.getGroup().setIsInner(testData.isCurrentUserFromInnerGroup);
        currentUser.getRole().setName(testData.nameOfRoleOfCurrentUser);
        if (testData.isCurrentUserEqualAuthorOfTicket) {
            currentUser = ticket.getAuthor();
        } else {
            currentUser.setId(UUID.randomUUID());
        }
        if (testData.isGroupOfCurrentUserEqualGroupOfAuthorOfTicket) {
            currentUser.setGroup(ticket.getAuthor().getGroup());
        } else {
            currentUser.getGroup().setId(UUID.randomUUID());
        }
        if (testData.isTicketsObserversContainsCurrentUser) {
            ticket.getObservers().add(currentUser);
        }
        if (testData.isTicketsExecutorContainsCurrentUser()) {
            ticket.getExecutors().add(currentUser);
        }
        var finalCurrentUser = currentUser;
        var actualException = assertThrows(AccessDeniedException.class,
                () -> validator.checkAccessBeforeCreate(ticket, finalCurrentUser)
        );
        assertEquals(expectedExceptionMessage, actualException.getMessage());
    }

    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    @Test
    void checkAccessForCreateAndUpdate_ShouldGetAccessDeniedException_whenCurrentUserHasNotPermitToTicketType() {
        var ticket = ticketTestHelper.getRandomValidEntity();
        var currentUser = ticket.getAuthor().toBuilder().build();
        currentUser.setGroup(ticket.getGroup().toBuilder().build());
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(false);
        AccessDeniedException actualException =
                assertThrows(
                        AccessDeniedException.class,
                        () -> validator.checkAccessBeforeCreate(ticket, currentUser)
                );
        assertEquals(ACCESS_DENIED_BECAUSE_CURRENT_USER_HAS_NOT_PERMIT_TO_TICKET_TYPE, actualException.getMessage());
        verify(settingsAccessToTicketTypesService, times(1)).isPermittedTicketType(any(), any());
    }

    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    @ParameterizedTest(name = "{index} - id of TestDataCrudTicketPermission ")
    @MethodSource("getTestDataForCheckAccessForReadWhenAccessDeniedException")
    void checkAccessForRead_ShouldGetAccessDeniedException_whenAccessDenied
            (TestDataCrudTicketPermission testData) {
        var expectedExceptionMessage = testData.exceptionMessage;
        var ticket = ticketTestHelper.getRandomValidEntity();
        var currentUser = ticket.getAuthor().toBuilder().build();
        currentUser.setGroup(ticket.getGroup().toBuilder().build());
        ticket.getAuthor().getGroup().setIsInner(testData.isAuthorOfTicketFromInnerGroup);
        currentUser.getGroup().setIsInner(testData.isCurrentUserFromInnerGroup);
        currentUser.getRole().setName(testData.nameOfRoleOfCurrentUser);
        if (testData.isCurrentUserEqualAuthorOfTicket) {
            currentUser = ticket.getAuthor();
        } else {
            currentUser.setId(UUID.randomUUID());
        }
        if (testData.isGroupOfCurrentUserEqualGroupOfAuthorOfTicket) {
            currentUser.setGroup(ticket.getAuthor().getGroup());
        } else {
            currentUser.getGroup().setId(UUID.randomUUID());
        }
        if (testData.isTicketsObserversContainsCurrentUser) {
            ticket.setObservers(List.of(currentUser));
        }
        if (testData.isTicketsExecutorContainsCurrentUser()) {
            ticket.setExecutors(List.of(currentUser));
        }
        var finalCurrentUser = currentUser;
        var actualException = assertThrows(AccessDeniedException.class,
                () -> validator.checkAccessBeforeRead(ticket, finalCurrentUser)
        );
        assertEquals(expectedExceptionMessage, actualException.getMessage());
    }

    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    @Test
    void checkAccessForRead_ShouldGetAccessDeniedException_whenCurrentUserHasNotPermitToTicketType() {
        var ticket = ticketTestHelper.getRandomValidEntity();
        var currentUser = ticket.getAuthor().toBuilder().build();
        currentUser.setGroup(ticket.getGroup().toBuilder().build());
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(false);
        AccessDeniedException actualException =
                assertThrows(
                        AccessDeniedException.class,
                        () -> validator.checkAccessBeforeRead(ticket, currentUser)
                );
        assertEquals(ACCESS_DENIED_BECAUSE_CURRENT_USER_HAS_NOT_PERMIT_TO_TICKET_TYPE, actualException.getMessage());
        verify(settingsAccessToTicketTypesService, times(1)).isPermittedTicketType(any(), any());
    }

    private static Stream<Arguments> getTestDataForCheckAccessForCreateAndUpdateWhenAccessDeniedException() {
        return Stream.of(
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .id(1)
                                .nameOfRoleOfCurrentUser(Roles.ADMIN.toString())
                                .isCurrentUserFromInnerGroup(false)
                                .isAuthorOfTicketFromInnerGroup(true)
                                .exceptionMessage(
                                        TicketOperationValidator.CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .id(2)
                                .nameOfRoleOfCurrentUser(Roles.ADMIN.toString())
                                .isCurrentUserFromInnerGroup(false)
                                .isAuthorOfTicketFromInnerGroup(false)
                                .exceptionMessage(
                                        TicketOperationValidator.CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .id(3)
                                .nameOfRoleOfCurrentUser(Roles.EXECUTOR.toString())
                                .isCurrentUserFromInnerGroup(false)
                                .isAuthorOfTicketFromInnerGroup(true)
                                .exceptionMessage(
                                        TicketOperationValidator.CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .id(4)
                                .nameOfRoleOfCurrentUser(Roles.EXECUTOR.toString())
                                .isCurrentUserFromInnerGroup(false)
                                .isAuthorOfTicketFromInnerGroup(false)
                                .exceptionMessage(
                                        TicketOperationValidator.CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .id(5)
                                .nameOfRoleOfCurrentUser(Roles.AUTHOR.toString())
                                .isCurrentUserFromInnerGroup(true)
                                .isAuthorOfTicketFromInnerGroup(true)
                                .exceptionMessage(
                                        TicketOperationValidator.CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_NOT_CURRENT_USER)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .id(6)
                                .nameOfRoleOfCurrentUser(Roles.AUTHOR.toString())
                                .isCurrentUserFromInnerGroup(false)
                                .isAuthorOfTicketFromInnerGroup(false)
                                .exceptionMessage(
                                        TicketOperationValidator.CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_NOT_CURRENT_USER)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .id(7)
                                .nameOfRoleOfCurrentUser(Roles.AUTHOR.toString())
                                .isCurrentUserFromInnerGroup(false)
                                .isAuthorOfTicketFromInnerGroup(true)
                                .exceptionMessage(
                                        TicketOperationValidator.CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .id(8)
                                .nameOfRoleOfCurrentUser(Roles.AUTHOR.toString())
                                .isCurrentUserFromInnerGroup(true)
                                .isAuthorOfTicketFromInnerGroup(false)
                                .exceptionMessage(
                                        TicketOperationValidator.CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_OUTER_GROUP)
                                .build()
                )
        );
    }

    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    @ParameterizedTest(name = "{index} - id of TestDataCrudTicketPermission ")
    @MethodSource("getTestDataForCheckAccessForCreateAndUpdateWhenAccessIsAllowed")
    void checkAccessForCreateAndUpdate_ShouldGetTrue_whenAccessIsAllowed
            (TestDataCrudTicketPermission testData) {
        var ticket = ticketTestHelper.getRandomValidEntity();
        var currentUser = ticket.getAuthor().toBuilder().build();
        currentUser.setGroup(ticket.getGroup().toBuilder().build());
        ticket.getAuthor().getGroup().setIsInner(testData.isAuthorOfTicketFromInnerGroup);
        currentUser.getGroup().setIsInner(testData.isCurrentUserFromInnerGroup);
        currentUser.getRole().setName(testData.nameOfRoleOfCurrentUser);
        if (testData.isCurrentUserEqualAuthorOfTicket) {
            currentUser = ticket.getAuthor();
        } else {
            currentUser.setId(UUID.randomUUID());
        }
        if (testData.isGroupOfCurrentUserEqualGroupOfAuthorOfTicket) {
            currentUser.setGroup(ticket.getAuthor().getGroup());
        } else {
            currentUser.getGroup().setId(UUID.randomUUID());
        }
        if (testData.isTicketsObserversContainsCurrentUser) {
            ticket.getObservers().add(currentUser);
        }
        if (testData.isTicketsExecutorContainsCurrentUser()) {
            ticket.getExecutors().add(currentUser);
        }
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(true);
        assertTrue(validator.checkAccessForCreateAndUpdate(ticket, currentUser));
        verify(settingsAccessToTicketTypesService, times(1)).isPermittedTicketType(any(), any());
    }

    private static Stream<Arguments> getTestDataForCheckAccessForReadWhenAccessDeniedException() {
        return Stream.of(
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .id(1)
                                .nameOfRoleOfCurrentUser(Roles.ADMIN.toString())
                                .isCurrentUserFromInnerGroup(false)
                                .isAuthorOfTicketFromInnerGroup(true)
                                .exceptionMessage(
                                        TicketOperationValidator.CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .id(2)
                                .nameOfRoleOfCurrentUser(Roles.ADMIN.toString())
                                .isCurrentUserFromInnerGroup(false)
                                .isAuthorOfTicketFromInnerGroup(false)
                                .exceptionMessage(
                                        TicketOperationValidator.CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .id(3)
                                .nameOfRoleOfCurrentUser(Roles.EXECUTOR.toString())
                                .isCurrentUserFromInnerGroup(false)
                                .isAuthorOfTicketFromInnerGroup(true)
                                .exceptionMessage(
                                        TicketOperationValidator.CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .id(4)
                                .nameOfRoleOfCurrentUser(Roles.EXECUTOR.toString())
                                .isCurrentUserFromInnerGroup(false)
                                .isAuthorOfTicketFromInnerGroup(false)
                                .exceptionMessage(
                                        TicketOperationValidator.CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .id(6)
                                .nameOfRoleOfCurrentUser(Roles.AUTHOR.toString())
                                .isCurrentUserFromInnerGroup(true)
                                .isAuthorOfTicketFromInnerGroup(false)
                                .exceptionMessage(
                                        TicketOperationValidator.CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_OUTER_GROUP)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .id(7)
                                .nameOfRoleOfCurrentUser(Roles.AUTHOR.toString())
                                .isCurrentUserFromInnerGroup(true)
                                .isAuthorOfTicketFromInnerGroup(true)
                                .isCurrentUserEqualAuthorOfTicket(false)
                                .isTicketsObserversContainsCurrentUser(false)
                                .exceptionMessage(
                                        TicketOperationValidator.CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_AS_A_AUTHOR_AND_HAS_NOT_IN_OBSERVERS)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .id(8)
                                .nameOfRoleOfCurrentUser(Roles.AUTHOR.toString())
                                .isCurrentUserFromInnerGroup(false)
                                .isAuthorOfTicketFromInnerGroup(true)
                                .exceptionMessage(
                                        TicketOperationValidator.CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .id(9)
                                .nameOfRoleOfCurrentUser(Roles.AUTHOR.toString())
                                .isCurrentUserFromInnerGroup(false)
                                .isAuthorOfTicketFromInnerGroup(false)
                                .isCurrentUserEqualAuthorOfTicket(false)
                                .isTicketsObserversContainsCurrentUser(false)
                                .exceptionMessage(
                                        TicketOperationValidator.CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_AS_A_AUTHOR_AND_HAS_NOT_IN_OBSERVERS)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .id(10)
                                .nameOfRoleOfCurrentUser(Roles.OBSERVER.toString())
                                .isTicketsObserversContainsCurrentUser(false)
                                .exceptionMessage(
                                        TicketOperationValidator.CURRENT_USER_WITH_ROLE_OBSERVER_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_IN_OBSERVERS)
                                .build()
                )
        );
    }

    private static Stream<Arguments> getTestDataForCheckAccessForCreateAndUpdateWhenAccessIsAllowed() {
        return Stream.of(
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .nameOfRoleOfCurrentUser(Roles.ACCOUNT_OWNER.toString())
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .nameOfRoleOfCurrentUser(Roles.ADMIN.toString())
                                .isCurrentUserFromInnerGroup(true)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .nameOfRoleOfCurrentUser(Roles.EXECUTOR.toString())
                                .isCurrentUserFromInnerGroup(true)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .nameOfRoleOfCurrentUser(Roles.AUTHOR.toString())
                                .isCurrentUserFromInnerGroup(true)
                                .isCurrentUserEqualAuthorOfTicket(true)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .nameOfRoleOfCurrentUser(Roles.AUTHOR.toString())
                                .isCurrentUserFromInnerGroup(false)
                                .isCurrentUserEqualAuthorOfTicket(true)
                                .build()
                )
        );
    }

    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    @ParameterizedTest(name = "{index} - id of TestDataCrudTicketPermission ")
    @MethodSource("getTestDataForCheckAccessForReadWhenAccessIsAllowed")
    void checkAccessForRead_ShouldNotGetException_whenAccessIsAllowed
            (TestDataCrudTicketPermission testData) {
        var ticket = ticketTestHelper.getRandomValidEntity();
        var currentUser = ticket.getAuthor().toBuilder().build();
        currentUser.setGroup(ticket.getGroup().toBuilder().build());
        ticket.getAuthor().getGroup().setIsInner(testData.isAuthorOfTicketFromInnerGroup);
        currentUser.getGroup().setIsInner(testData.isCurrentUserFromInnerGroup);
        currentUser.getRole().setName(testData.nameOfRoleOfCurrentUser);
        if (testData.isCurrentUserEqualAuthorOfTicket) {
            currentUser = ticket.getAuthor();
        } else {
            currentUser.setId(UUID.randomUUID());
        }
        if (testData.isGroupOfCurrentUserEqualGroupOfAuthorOfTicket) {
            currentUser.setGroup(ticket.getAuthor().getGroup());
        } else {
            currentUser.getGroup().setId(UUID.randomUUID());
        }
        if (testData.isTicketsObserversContainsCurrentUser) {
            ticket.setObservers(List.of(currentUser));
        }
        if (testData.isTicketsExecutorContainsCurrentUser()) {
            ticket.setExecutors(List.of(currentUser));
        }
        when(settingsAccessToTicketTypesService.isPermittedTicketType(any(), any())).thenReturn(true);
        validator.checkAccessBeforeRead(ticket, currentUser);
        verify(settingsAccessToTicketTypesService, times(1)).isPermittedTicketType(any(), any());
    }

    private static Stream<Arguments> getTestDataForCheckAccessForReadWhenAccessIsAllowed() {
        return Stream.of(
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .nameOfRoleOfCurrentUser(Roles.ADMIN.toString())
                                .isCurrentUserFromInnerGroup(true)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .nameOfRoleOfCurrentUser(Roles.ADMIN.toString())
                                .isCurrentUserFromInnerGroup(false)
                                .isAuthorOfTicketFromInnerGroup(false)
                                .isGroupOfCurrentUserEqualGroupOfAuthorOfTicket(true)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .nameOfRoleOfCurrentUser(Roles.EXECUTOR.toString())
                                .isCurrentUserFromInnerGroup(true)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .nameOfRoleOfCurrentUser(Roles.EXECUTOR.toString())
                                .isCurrentUserFromInnerGroup(false)
                                .isAuthorOfTicketFromInnerGroup(false)
                                .isGroupOfCurrentUserEqualGroupOfAuthorOfTicket(true)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .nameOfRoleOfCurrentUser(Roles.AUTHOR.toString())
                                .isCurrentUserFromInnerGroup(true)
                                .isAuthorOfTicketFromInnerGroup(true)
                                .isCurrentUserEqualAuthorOfTicket(true)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .nameOfRoleOfCurrentUser(Roles.AUTHOR.toString())
                                .isCurrentUserFromInnerGroup(false)
                                .isAuthorOfTicketFromInnerGroup(false)
                                .isCurrentUserEqualAuthorOfTicket(true)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .nameOfRoleOfCurrentUser(Roles.OBSERVER.toString())
                                .isTicketsObserversContainsCurrentUser(true)
                                .build()
                )

        );
    }
}
