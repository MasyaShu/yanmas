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
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.EMPTY_TICKET;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.MUST_NOT_CREATE_UPDATE_TICKET_IF_SUBJECT_DESCRIPTION_AND_FILES_ARE_EMPTY;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR_LESS_THAN_WEIGHT_OF_ROLE_AUTHOR;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS_LESS_THAN_WEIGHT_OF_ROLE_EXECUTOR;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS_LESS_THAN_WEIGHT_OF_ROLE_OBSERVER;

import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.test.RoleTestHelper;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
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
    void beforeCreate_shouldGetLogicalValidationException_whenSubjectDescriptionFilesAreEmpty() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        ticket.setSubject("");
        ticket.setDescription("");
        when(userService.findByEmail(any())).thenReturn(Optional.of(ticket.getAuthor()));
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
    void beforeCreate_shouldGetTrue_whenPassedTicketIsValid() {
        var ticket = ticketTestHelper.getRandomValidEntity();
        when(userService.findByEmail(any())).thenReturn(Optional.of(ticket.getAuthor()));
        var actualResult = validator.beforeCreate(ticket);
        assertTrue(actualResult);
        verify(userService, times(1)).findByEmail(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenSubjectDescriptionFilesAreEmpty() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        ticket.setSubject("");
        ticket.setDescription("");
        when(userService.findByEmail(any())).thenReturn(Optional.of(ticket.getAuthor()));
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
        when(userService.findByEmail(any())).thenReturn(Optional.of(ticket.getAuthor()));
        var actualResult = validator.beforeUpdate(ticket);
        assertTrue(actualResult);
        verify(userService, times(1)).findByEmail(any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetLogicalValidationException_whenWeightOfRoleOfAuthorLessThanAccordingWeightOfRole() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        ticket.getAuthor().setRole(roleTestHelper.getPredefinedValidEntityList().get(4));
        when(userService.findByEmail(any())).thenReturn(Optional.of(ticket.getAuthor()));
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
        when(userService.findByEmail(any())).thenReturn(Optional.of(ticket.getAuthor()));
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
        when(userService.findByEmail(any())).thenReturn(Optional.of(ticket.getAuthor()));
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
    void beforeUpdate_shouldGetLogicalValidationException_whenWeightOfRoleOfAuthorLessThanAccordingWeightOfRole() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        ticket.getAuthor().setRole(roleTestHelper.getPredefinedValidEntityList().get(4));
        when(userService.findByEmail(any())).thenReturn(Optional.of(ticket.getAuthor()));
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
        when(userService.findByEmail(any())).thenReturn(Optional.of(ticket.getAuthor()));
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
        when(userService.findByEmail(any())).thenReturn(Optional.of(ticket.getAuthor()));
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

    @ParameterizedTest
    @ValueSource(ints = {1, 4, 5, -3, 15, Integer.MAX_VALUE})
    void isOdd_ShouldReturnTrueForOddNumbers(int number) {
        assertTrue(number % 2 == 0);
    }

}