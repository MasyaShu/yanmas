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
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.ACCOUNTS_ARE_DIFFERENT;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.ACCOUNT_OF_TICKET_IS_NOT_EQUAL_FOR_THE_FOLLOWING_FIELDS;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.EMPTY_TICKET;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.GROUP_OF_TICKET;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.GROUP_OF_TICKET_MUST_EQUALS_GROUP_OF_AUTHOR_OF_TICKET;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.MUST_NOT_CREATE_UPDATE_TICKET_IF_SUBJECT_DESCRIPTION_AND_FILES_ARE_EMPTY;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.USER_FROM_NOT_INNER_GROUP_MUST_CREATE_UPDATE_TICKET_ONLY_WITH_HIS_GROUP;
import static ru.itterminal.botdesk.tickets.service.validator.TicketOperationValidator.USER_IS_NOT_FROM_INNER_GROUP;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.aau.model.test.AccountTestHelper;
import ru.itterminal.botdesk.aau.model.test.GroupTestHelper;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.security.config.TestSecurityConfig;
import ru.itterminal.botdesk.tickets.model.test.TicketTestHelper;

@SpringJUnitConfig(value = {TicketOperationValidator.class})
@Import(TestSecurityConfig.class)
@ActiveProfiles("Test")
class TicketOperationValidatorTest {

    @MockBean
    private GroupServiceImpl groupService;

    @Autowired
    private final TicketOperationValidator validator = new TicketOperationValidator(groupService);

    private final TicketTestHelper ticketTestHelper = new TicketTestHelper();
    private final GroupTestHelper groupTestHelper = new GroupTestHelper();
    private final AccountTestHelper accountTestHelper = new AccountTestHelper();

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetLogicalValidationException_whenSubjectDescriptionFilesAreEmpty() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        ticket.setSubject("");
        ticket.setDescription("");
        when(groupService.findByIdAndAccountId(any(), any())).thenReturn(ticket.getGroup());
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
        verify(groupService, times(1)).findByIdAndAccountId(any(), any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetTrue_whenPassedTicketIsValid() {
        var ticket = ticketTestHelper.getRandomValidEntity();
        when(groupService.findByIdAndAccountId(any(), any())).thenReturn(ticket.getGroup());
        var actualResult = validator.beforeCreate(ticket);
        assertTrue(actualResult);
        verify(groupService, times(1)).findByIdAndAccountId(any(), any());
        // Account of ticket is not equal for the following fields: group, author, ticketStatus, ticketType, ticketTemplate
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenSubjectDescriptionFilesAreEmpty() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        ticket.setSubject("");
        ticket.setDescription("");
        when(groupService.findByIdAndAccountId(any(), any())).thenReturn(ticket.getGroup());

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
        verify(groupService, times(1)).findByIdAndAccountId(any(), any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetTrue_whenPassedTicketIsValid() {
        var ticket = ticketTestHelper.getRandomValidEntity();
        when(groupService.findByIdAndAccountId(any(), any())).thenReturn(ticket.getGroup());
        var actualResult = validator.beforeUpdate(ticket);
        assertTrue(actualResult);
        verify(groupService, times(1)).findByIdAndAccountId(any(), any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeCreate_shouldGetLogicalValidationException_whenGroupOfCurrentUserIsNotInnerGroupAndGroupOfTicketIsInnerGroup() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        var groupOfCurrentUser = groupTestHelper.getRandomValidEntity();
        groupOfCurrentUser.setIsInner(false);
        when(groupService.findByIdAndAccountId(any(), any())).thenReturn(groupOfCurrentUser);
        expectedErrors.put(
                USER_IS_NOT_FROM_INNER_GROUP,
                singletonList(
                        new ValidationError(
                                USER_IS_NOT_FROM_INNER_GROUP,
                                USER_FROM_NOT_INNER_GROUP_MUST_CREATE_UPDATE_TICKET_ONLY_WITH_HIS_GROUP
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
                expectedException.getFieldErrors().get(USER_IS_NOT_FROM_INNER_GROUP).get(0),
                actualException.getFieldErrors().get(USER_IS_NOT_FROM_INNER_GROUP).get(0)
        );
        verify(groupService, times(1)).findByIdAndAccountId(any(), any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenGroupOfCurrentUserIsNotInnerGroupAndGroupOfTicketIsInnerGroup() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        var groupOfCurrentUser = groupTestHelper.getRandomValidEntity();
        groupOfCurrentUser.setIsInner(false);
        when(groupService.findByIdAndAccountId(any(), any())).thenReturn(groupOfCurrentUser);
        expectedErrors.put(
                USER_IS_NOT_FROM_INNER_GROUP,
                singletonList(
                        new ValidationError(
                                USER_IS_NOT_FROM_INNER_GROUP,
                                USER_FROM_NOT_INNER_GROUP_MUST_CREATE_UPDATE_TICKET_ONLY_WITH_HIS_GROUP
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
                expectedException.getFieldErrors().get(USER_IS_NOT_FROM_INNER_GROUP).get(0),
                actualException.getFieldErrors().get(USER_IS_NOT_FROM_INNER_GROUP).get(0)
        );
        verify(groupService, times(1)).findByIdAndAccountId(any(), any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetLogicalValidationException_whenGroupOfTicketIsNotEqualGroupOfAuthorOfTicket() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        var groupOfAuthor = groupTestHelper.getRandomValidEntity();
        groupOfAuthor.setIsInner(true);
        groupOfAuthor.setAccount(ticket.getAccount());
        var author = ticket.getAuthor();
        author.setGroup(groupOfAuthor);
        ticket.setAuthor(author);
        when(groupService.findByIdAndAccountId(any(), any())).thenReturn(groupOfAuthor);
        expectedErrors.put(
                GROUP_OF_TICKET,
                singletonList(
                        new ValidationError(
                                GROUP_OF_TICKET,
                                GROUP_OF_TICKET_MUST_EQUALS_GROUP_OF_AUTHOR_OF_TICKET
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
                expectedException.getFieldErrors().get(GROUP_OF_TICKET).get(0),
                actualException.getFieldErrors().get(GROUP_OF_TICKET).get(0)
        );
        verify(groupService, times(1)).findByIdAndAccountId(any(), any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenGroupOfTicketIsNotEqualGroupOfAuthorOfTicket() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        var groupOfAuthor = groupTestHelper.getRandomValidEntity();
        groupOfAuthor.setIsInner(true);
        groupOfAuthor.setAccount(ticket.getAccount());
        var author = ticket.getAuthor();
        author.setGroup(groupOfAuthor);
        ticket.setAuthor(author);
        when(groupService.findByIdAndAccountId(any(), any())).thenReturn(groupOfAuthor);
        expectedErrors.put(
                GROUP_OF_TICKET,
                singletonList(
                        new ValidationError(
                                GROUP_OF_TICKET,
                                GROUP_OF_TICKET_MUST_EQUALS_GROUP_OF_AUTHOR_OF_TICKET
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
                expectedException.getFieldErrors().get(GROUP_OF_TICKET).get(0),
                actualException.getFieldErrors().get(GROUP_OF_TICKET).get(0)
        );
        verify(groupService, times(1)).findByIdAndAccountId(any(), any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetLogicalValidationException_whenAccountOfTicketIsNotEqualForTheNestedFields() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        ticket.getAuthor().setAccount(accountTestHelper.getRandomValidEntity());
        ticket.getGroup().setAccount(accountTestHelper.getRandomValidEntity());
        ticket.setTicketStatus(null);
        ticket.setTicketTemplate(null);
        ticket.setTicketType(null);
        when(groupService.findByIdAndAccountId(any(), any())).thenReturn(ticket.getGroup());
        expectedErrors.put(
                ACCOUNTS_ARE_DIFFERENT,
                singletonList(
                        new ValidationError(
                                ACCOUNTS_ARE_DIFFERENT,
                                format(
                                        ACCOUNT_OF_TICKET_IS_NOT_EQUAL_FOR_THE_FOLLOWING_FIELDS,
                                        "group, author"
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
                expectedException.getFieldErrors().get(ACCOUNTS_ARE_DIFFERENT).get(0),
                actualException.getFieldErrors().get(ACCOUNTS_ARE_DIFFERENT).get(0)
        );
        verify(groupService, times(1)).findByIdAndAccountId(any(), any());
    }

    @Test
    @WithUserDetails("AUTHOR_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetLogicalValidationException_whenAccountOfTicketIsNotEqualForTheNestedFields() {
        var expectedErrors = createMapForLogicalErrors();
        var ticket = ticketTestHelper.getRandomValidEntity();
        ticket.getAuthor().setAccount(accountTestHelper.getRandomValidEntity());
        ticket.getGroup().setAccount(accountTestHelper.getRandomValidEntity());
        ticket.setTicketStatus(null);
        ticket.setTicketTemplate(null);
        ticket.setTicketType(null);
        when(groupService.findByIdAndAccountId(any(), any())).thenReturn(ticket.getGroup());
        expectedErrors.put(
                ACCOUNTS_ARE_DIFFERENT,
                singletonList(
                        new ValidationError(
                                ACCOUNTS_ARE_DIFFERENT,
                                format(
                                        ACCOUNT_OF_TICKET_IS_NOT_EQUAL_FOR_THE_FOLLOWING_FIELDS,
                                        "group, author"
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
                expectedException.getFieldErrors().get(ACCOUNTS_ARE_DIFFERENT).get(0),
                actualException.getFieldErrors().get(ACCOUNTS_ARE_DIFFERENT).get(0)
        );
        verify(groupService, times(1)).findByIdAndAccountId(any(), any());
    }

}