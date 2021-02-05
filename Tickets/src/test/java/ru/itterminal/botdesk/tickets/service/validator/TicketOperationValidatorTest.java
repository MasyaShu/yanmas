package ru.itterminal.botdesk.tickets.service.validator;

import static java.lang.String.format;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.VALIDATION_FAILED;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.botdesk.security.config.TestSecurityConfig.AUTHOR_ACCOUNT_1_IS_INNER_GROUP_ID;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_OUTER_GROUP;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_NOT_CURRENT_USER;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_OUTER_GROUP;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_AS_A_AUTHOR_AND_HAS_NOT_IN_OBSERVERS;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_NOT_CURRENT_USER;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_AS_A_AUTHOR_AND_HAS_NOT_IN_OBSERVERS;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.CURRENT_USER_WITH_ROLE_OBSERVER_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_IN_OBSERVERS;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.EMPTY_TICKET;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.FILE_ALREADY_HAS_A_LINK_TO_ANOTHER_ENTITY;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.FILE_IS_INVALID;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.FILE_IS_NOT_YET_UPLOADED;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.FILE_WAS_CREATED_BY_ANOTHER_USER_YOU_CANNOT_USE_IT_FOR_CREATE_THIS_TICKET;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.MUST_NOT_CREATE_UPDATE_TICKET_IF_SUBJECT_DESCRIPTION_AND_FILES_ARE_EMPTY;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR_LESS_THAN_WEIGHT_OF_ROLE_AUTHOR;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS_LESS_THAN_WEIGHT_OF_ROLE_EXECUTOR;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS_LESS_THAN_WEIGHT_OF_ROLE_OBSERVER;

import java.util.List;
import java.util.UUID;
import java.util.stream.Stream;

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

import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.test.RoleTestHelper;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.files.model.File;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.tickets.model.test.TicketTestHelper;

@SpringJUnitConfig(value = {TicketOperationValidator.class})
@Import(TestSecurityConfig.class)
@ActiveProfiles("Test")
class TicketOperationValidatorTest {

    @MockBean
    private UserServiceImpl userService;

    @Autowired
    private final TicketOperationValidator validator = new TicketOperationValidator(userService);

    private final TicketTestHelper ticketTestHelper = new TicketTestHelper();
    private final RoleTestHelper roleTestHelper = new RoleTestHelper();

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetTrue_whenPassedTicketIsValid() {
        var ticket = ticketTestHelper.getRandomValidEntity();
        when(userService.findByEmail(any())).thenReturn(ticket.getAuthor());
        var actualResult = validator.beforeCreate(ticket);
        assertTrue(actualResult);
        verify(userService, times(1)).findByEmail(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetLogicalValidationException_whenSubjectDescriptionFilesAreEmpty() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        ticket.setSubject("");
        ticket.setDescription("");
        when(userService.findByEmail(any())).thenReturn(ticket.getAuthor());
        expectedErrors.put(
                EMPTY_TICKET,
                singletonList(
                        new ValidationError(
                                EMPTY_TICKET,
                                MUST_NOT_CREATE_UPDATE_TICKET_IF_SUBJECT_DESCRIPTION_AND_FILES_ARE_EMPTY
                        )
                )
        );
        LogicalValidationException expectedException =
                new LogicalValidationException(VALIDATION_FAILED, expectedErrors);
        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.beforeCreate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(EMPTY_TICKET).get(0),
                actualException.getFieldErrors().get(EMPTY_TICKET).get(0)
        );
        verify(userService, times(1)).findByEmail(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetLogicalValidationException_whenWeightOfRoleOfAuthorLessThanAccordingWeightOfRole() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        ticket.getAuthor().setRole(roleTestHelper.getPredefinedValidEntityList().get(4));
        when(userService.findByEmail(any())).thenReturn(ticket.getAuthor());
        expectedErrors.put(
                WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR,
                singletonList(
                        new ValidationError(
                                WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR,
                                format(
                                        WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR_LESS_THAN_WEIGHT_OF_ROLE_AUTHOR,
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
                        () -> validator.beforeCreate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR).get(0),
                actualException.getFieldErrors().get(WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR).get(0)
        );
        verify(userService, times(1)).findByEmail(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetLogicalValidationException_whenWeightOfRoleOfObserverLessThanAccordingWeightOfRole() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        var observer = ticket.getAuthor();
        observer.getRole().setWeight(0);
        ticket.setObservers(List.of(observer));
        when(userService.findByEmail(any())).thenReturn(ticket.getAuthor());
        expectedErrors.put(
                WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS,
                singletonList(
                        new ValidationError(
                                WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS,
                                format(
                                        WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS_LESS_THAN_WEIGHT_OF_ROLE_OBSERVER,
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
                        () -> validator.beforeCreate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS).get(0),
                actualException.getFieldErrors().get(WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS).get(0)
        );
        verify(userService, times(1)).findByEmail(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetLogicalValidationException_whenWeightOfRoleOfExecutorLessThanAccordingWeightOfRole() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        var executor = ticket.getAuthor();
        executor.getRole().setWeight(0);
        ticket.setExecutors(List.of(executor));
        when(userService.findByEmail(any())).thenReturn(ticket.getAuthor());
        expectedErrors.put(
                WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS,
                singletonList(
                        new ValidationError(
                                WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS,
                                format(
                                        WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS_LESS_THAN_WEIGHT_OF_ROLE_EXECUTOR,
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
                        () -> validator.beforeCreate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS).get(0),
                actualException.getFieldErrors().get(WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS).get(0)
        );
        verify(userService, times(1)).findByEmail(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetLogicalValidationException_whenPassedFileHasLinkToAnotherTicket() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        var invalidFile = File.builder()
                .entityId(UUID.randomUUID())
                .authorId(UUID.fromString(AUTHOR_ACCOUNT_1_IS_INNER_GROUP_ID))
                .isUploaded(true)
                .build();
        ticket.setFiles(List.of(invalidFile));
        when(userService.findByEmail(any())).thenReturn(ticket.getAuthor());
        expectedErrors.put(
                FILE_IS_INVALID,
                singletonList(
                        new ValidationError(
                                FILE_IS_INVALID,
                                FILE_ALREADY_HAS_A_LINK_TO_ANOTHER_ENTITY
                        )
                )
        );
        LogicalValidationException expectedException =
                new LogicalValidationException(VALIDATION_FAILED, expectedErrors);
        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.beforeCreate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(FILE_IS_INVALID).get(0),
                actualException.getFieldErrors().get(FILE_IS_INVALID).get(0)
        );
        verify(userService, times(1)).findByEmail(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetLogicalValidationException_whenFileWasCreatedByAnotherUser() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        var invalidFile = File.builder()
                .entityId(null)
                .authorId(UUID.randomUUID())
                .isUploaded(true)
                .build();
        ticket.setFiles(List.of(invalidFile));
        when(userService.findByEmail(any())).thenReturn(ticket.getAuthor());
        expectedErrors.put(
                FILE_IS_INVALID,
                singletonList(
                        new ValidationError(
                                FILE_IS_INVALID,
                                FILE_WAS_CREATED_BY_ANOTHER_USER_YOU_CANNOT_USE_IT_FOR_CREATE_THIS_TICKET
                        )
                )
        );
        LogicalValidationException expectedException =
                new LogicalValidationException(VALIDATION_FAILED, expectedErrors);
        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.beforeCreate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(FILE_IS_INVALID).get(0),
                actualException.getFieldErrors().get(FILE_IS_INVALID).get(0)
        );
        verify(userService, times(1)).findByEmail(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetLogicalValidationException_whenFileIsNotYetUploaded() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        var invalidFile = File.builder()
                .entityId(null)
                .authorId(UUID.fromString(AUTHOR_ACCOUNT_1_IS_INNER_GROUP_ID))
                .isUploaded(false)
                .build();
        ticket.setFiles(List.of(invalidFile));
        when(userService.findByEmail(any())).thenReturn(ticket.getAuthor());
        expectedErrors.put(
                FILE_IS_INVALID,
                singletonList(
                        new ValidationError(
                                FILE_IS_INVALID,
                                FILE_IS_NOT_YET_UPLOADED
                        )
                )
        );
        LogicalValidationException expectedException =
                new LogicalValidationException(VALIDATION_FAILED, expectedErrors);
        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.beforeCreate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(FILE_IS_INVALID).get(0),
                actualException.getFieldErrors().get(FILE_IS_INVALID).get(0)
        );
        verify(userService, times(1)).findByEmail(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenSubjectDescriptionFilesAreEmpty() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        ticket.setSubject("");
        ticket.setDescription("");
        when(userService.findByEmail(any())).thenReturn(ticket.getAuthor());
        expectedErrors.put(
                EMPTY_TICKET,
                singletonList(
                        new ValidationError(
                                EMPTY_TICKET,
                                MUST_NOT_CREATE_UPDATE_TICKET_IF_SUBJECT_DESCRIPTION_AND_FILES_ARE_EMPTY
                        )
                )
        );
        LogicalValidationException expectedException =
                new LogicalValidationException(VALIDATION_FAILED, expectedErrors);
        LogicalValidationException actualException =
                assertThrows(
                        LogicalValidationException.class,
                        () -> validator.beforeUpdate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(EMPTY_TICKET).get(0),
                actualException.getFieldErrors().get(EMPTY_TICKET).get(0)
        );
        verify(userService, times(1)).findByEmail(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetTrue_whenPassedTicketIsValid() {
        var ticket = ticketTestHelper.getRandomValidEntity();
        when(userService.findByEmail(any())).thenReturn(ticket.getAuthor());
        var actualResult = validator.beforeUpdate(ticket);
        assertTrue(actualResult);
        verify(userService, times(1)).findByEmail(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenWeightOfRoleOfAuthorLessThanAccordingWeightOfRole() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        ticket.getAuthor().setRole(roleTestHelper.getPredefinedValidEntityList().get(4));
        when(userService.findByEmail(any())).thenReturn(ticket.getAuthor());
        expectedErrors.put(
                WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR,
                singletonList(
                        new ValidationError(
                                WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR,
                                format(
                                        WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR_LESS_THAN_WEIGHT_OF_ROLE_AUTHOR,
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
                        () -> validator.beforeUpdate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR).get(0),
                actualException.getFieldErrors().get(WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR).get(0)
        );
        verify(userService, times(1)).findByEmail(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenWeightOfRoleOfObserverLessThanAccordingWeightOfRole() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        var observer = ticket.getAuthor();
        observer.getRole().setWeight(0);
        ticket.setObservers(List.of(observer));
        when(userService.findByEmail(any())).thenReturn(ticket.getAuthor());
        expectedErrors.put(
                WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS,
                singletonList(
                        new ValidationError(
                                WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS,
                                format(
                                        WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS_LESS_THAN_WEIGHT_OF_ROLE_OBSERVER,
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
                        () -> validator.beforeUpdate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS).get(0),
                actualException.getFieldErrors().get(WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS).get(0)
        );
        verify(userService, times(1)).findByEmail(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenWeightOfRoleOfExecutorLessThanAccordingWeightOfRole() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        var executor = ticket.getAuthor();
        executor.getRole().setWeight(0);
        ticket.setExecutors(List.of(executor));
        when(userService.findByEmail(any())).thenReturn(ticket.getAuthor());
        expectedErrors.put(
                WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS,
                singletonList(
                        new ValidationError(
                                WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS,
                                format(
                                        WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS_LESS_THAN_WEIGHT_OF_ROLE_EXECUTOR,
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
                        () -> validator.beforeUpdate(ticket)
                );
        assertEquals(
                expectedException.getFieldErrors().get(WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS).get(0),
                actualException.getFieldErrors().get(WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS).get(0)
        );
        verify(userService, times(1)).findByEmail(any());
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
        when(userService.findByEmail(any())).thenReturn(currentUser);
        AccessDeniedException actualException =
                assertThrows(
                        AccessDeniedException.class,
                        () -> validator.beforeCreate(ticket)
                );
        assertEquals(expectedExceptionMessage, actualException.getMessage());
        verify(userService, times(1)).findByEmail(any());
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
        when(userService.findByEmail(any())).thenReturn(currentUser);
        AccessDeniedException actualException =
                assertThrows(
                        AccessDeniedException.class,
                        () -> validator.checkAccessForRead(ticket)
                );
        assertEquals(expectedExceptionMessage, actualException.getMessage());
        verify(userService, times(1)).findByEmail(any());
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
                                        CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP)
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
                                        CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP)
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
                                        CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP)
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
                                        CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP)
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
                                        CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_NOT_CURRENT_USER)
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
                                        CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_NOT_CURRENT_USER)
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
                                        CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP)
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
                                        CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_OUTER_GROUP)
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
        when(userService.findByEmail(any())).thenReturn(currentUser);
        assertTrue(validator.checkAccessForCreateAndUpdate(ticket));
        verify(userService, times(1)).findByEmail(any());
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
                                        CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP)
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
                                        CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP)
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
                                        CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP)
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
                                        CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP)
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
                                        CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_OUTER_GROUP)
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
                                        CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_AS_A_AUTHOR_AND_HAS_NOT_IN_OBSERVERS)
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
                                        CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP)
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
                                        CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_AS_A_AUTHOR_AND_HAS_NOT_IN_OBSERVERS)
                                .build()
                ),
                Arguments.of(
                        TestDataCrudTicketPermission
                                .builder()
                                .id(10)
                                .nameOfRoleOfCurrentUser(Roles.OBSERVER.toString())
                                .isTicketsObserversContainsCurrentUser(false)
                                .exceptionMessage(
                                        CURRENT_USER_WITH_ROLE_OBSERVER_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_IN_OBSERVERS)
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
    void checkAccessForRead_ShouldGetTrue_whenAccessIsAllowed
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
        when(userService.findByEmail(any())).thenReturn(currentUser);
        assertTrue(validator.checkAccessForRead(ticket));
        verify(userService, times(1)).findByEmail(any());
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