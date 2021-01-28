package ru.itterminal.botdesk.tickets.service.validator;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.ifErrorsNotEmptyThrowLogicalValidationException;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.files.model.File;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.tickets.model.Ticket;

@Slf4j
@Component
@RequiredArgsConstructor
public class TicketOperationValidator extends BasicOperationValidatorImpl<Ticket> {

    public static final String USER_IS_NOT_FROM_INNER_GROUP = "User is not from inner group";
    public static final String USER_FROM_NOT_INNER_GROUP_MUST_CREATE_UPDATE_TICKET_ONLY_WITH_HIS_GROUP =
            "User from not inner group must create/update ticket only with his group";
    public static final String LOG_USER_FROM_NOT_INNER_GROUP =
            "User {} from not inner group must create/update ticket {} only with his group";
    public static final String GROUP_OF_TICKET = "Group of ticket";
    public static final String GROUP_OF_TICKET_MUST_EQUALS_GROUP_OF_AUTHOR_OF_TICKET =
            "Group of ticket must equals group of author of ticket";
    public static final String LOG_GROUP_OF_TICKET =
            "Group of ticket {} must equals group of author of ticket {}";
    public static final String ACCOUNTS_ARE_DIFFERENT = "Accounts are different";
    public static final String ACCOUNT_OF_TICKET_IS_NOT_EQUAL_FOR_THE_FOLLOWING_FIELDS =
            "Account of ticket is not equal for the following fields: %s";
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR = "Weight of role into field Author";
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS = "Weight of role into field Executors";
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS = "Weight of role into field Observers";
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR_LESS_THAN_WEIGHT_OF_ROLE_AUTHOR =
            "Weight of role (%s) into field Author less than weight of role Author (%s)";
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS_LESS_THAN_WEIGHT_OF_ROLE_EXECUTOR =
            "Weight of role (%s) into field Executors less than weight of role Executor (%s)";
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS_LESS_THAN_WEIGHT_OF_ROLE_OBSERVER =
            "Weight of role (%s) into field Observers less than weight of role Observer (%s)";
    private final GroupServiceImpl groupService;

    public static final String EMPTY_TICKET = "Empty ticket";
    public static final String LOG_EMPTY_TICKET = "Mustn't create/update ticket if subject, description and files are"
            + " empty: {}";
    public static final String MUST_NOT_CREATE_UPDATE_TICKET_IF_SUBJECT_DESCRIPTION_AND_FILES_ARE_EMPTY =
            "Mustn't create/update ticket if subject, description and files are empty";

    @Override
    public boolean beforeCreate(Ticket entity) {
        var result = super.beforeCreate(entity);
        var errors = createMapForLogicalErrors();
        IsEmptySubjectDescriptionAndFiles(entity, errors);
        checkGroupOfCurrentUserAndGroupFromTicket(entity, errors);
        checkAccountOfTicketAndAccountsFromAllNestedObjectsOfTicket(entity, errors);
        checkAuthorExecutorsAndObserversForWeightOfRoles(entity, errors);
        ifErrorsNotEmptyThrowLogicalValidationException(errors);
        return result;
    }

    @Override
    public boolean beforeUpdate(Ticket entity) {
        var result = super.beforeUpdate(entity);
        var errors = createMapForLogicalErrors();
        IsEmptySubjectDescriptionAndFiles(entity, errors);
        checkGroupOfCurrentUserAndGroupFromTicket(entity, errors);
        checkAccountOfTicketAndAccountsFromAllNestedObjectsOfTicket(entity, errors);
        checkAuthorExecutorsAndObserversForWeightOfRoles(entity, errors);
        ifErrorsNotEmptyThrowLogicalValidationException(errors);
        return result;
    }

    private void IsEmptySubjectDescriptionAndFiles(Ticket ticket, Map<String, List<ValidationError>> errors) {
        if ((ticket.getDescription() == null || ticket.getDescription().isEmpty())
                && (ticket.getSubject() == null || ticket.getSubject().isEmpty())
                && (ticket.getFiles() == null || ticket.getFiles().isEmpty())) {
            addValidationErrorIntoErrors(
                    EMPTY_TICKET,
                    MUST_NOT_CREATE_UPDATE_TICKET_IF_SUBJECT_DESCRIPTION_AND_FILES_ARE_EMPTY,
                    errors
            );
            log.error(LOG_EMPTY_TICKET, ticket);
        }
    }

    private void checkGroupOfCurrentUserAndGroupFromTicket(Ticket ticket, Map<String, List<ValidationError>> errors) {
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        var groupOfCurrentUser = groupService.findByIdAndAccountId(jwtUser.getGroupId(), jwtUser.getAccountId());
        var groupFromTicket = ticket.getGroup();
        var groupFromAuthorOfTicket = ticket.getAuthor().getGroup();
        if (Boolean.FALSE.equals(groupOfCurrentUser.getIsInner()) && !groupOfCurrentUser.equals(groupFromTicket)) {
            addValidationErrorIntoErrors(
                    USER_IS_NOT_FROM_INNER_GROUP,
                    USER_FROM_NOT_INNER_GROUP_MUST_CREATE_UPDATE_TICKET_ONLY_WITH_HIS_GROUP,
                    errors
            );
            log.error(LOG_USER_FROM_NOT_INNER_GROUP, jwtUser, ticket);
        }
        if (!groupFromAuthorOfTicket.equals(groupFromTicket)) {
            addValidationErrorIntoErrors(
                    GROUP_OF_TICKET,
                    GROUP_OF_TICKET_MUST_EQUALS_GROUP_OF_AUTHOR_OF_TICKET,
                    errors
            );
            log.error(LOG_GROUP_OF_TICKET, groupFromTicket, groupFromAuthorOfTicket);
        }
    }

    private void checkAccountOfTicketAndAccountsFromAllNestedObjectsOfTicket
            (Ticket ticket, Map<String, List<ValidationError>> errors) {
        List<String> problemFieldsOfTicket = new ArrayList<>();
        var accountOfTicket = ticket.getAccount();
        if (ticket.getGroup() != null && !ticket.getGroup().getAccount().equals(accountOfTicket)) {
            problemFieldsOfTicket.add("group");
        }
        if (ticket.getAuthor() != null && !ticket.getAuthor().getAccount().equals(accountOfTicket)) {
            problemFieldsOfTicket.add("author");
        }
        if (ticket.getTicketStatus() != null && !ticket.getTicketStatus().getAccount().equals(accountOfTicket)) {
            problemFieldsOfTicket.add("ticketStatus");
        }
        if (ticket.getTicketType() != null && !ticket.getTicketType().getAccount().equals(accountOfTicket)) {
            problemFieldsOfTicket.add("ticketType");
        }
        if (ticket.getTicketTemplate() != null && !ticket.getTicketTemplate().getAccount().equals(accountOfTicket)) {
            problemFieldsOfTicket.add("ticketTemplate");
        }
        if (ticket.getObservers() != null && !ticket.getObservers().isEmpty()) {
            for (User observer : ticket.getObservers()) {
                if (!observer.getAccount().equals(accountOfTicket)) {
                    problemFieldsOfTicket.add("observers");
                    break;
                }
            }
        }
        if (ticket.getExecutors() != null && !ticket.getExecutors().isEmpty()) {
            for (User executor : ticket.getExecutors()) {
                if (!executor.getAccount().equals(accountOfTicket)) {
                    problemFieldsOfTicket.add("executors");
                    break;
                }
            }
        }
        if (ticket.getFiles() != null && !ticket.getFiles().isEmpty()) {
            for (File file : ticket.getFiles()) {
                if (!file.getAccount().equals(accountOfTicket)) {
                    problemFieldsOfTicket.add("files");
                    break;
                }
            }
        }
        if (!problemFieldsOfTicket.isEmpty()) {
            addValidationErrorIntoErrors(
                    ACCOUNTS_ARE_DIFFERENT,
                    format(
                            ACCOUNT_OF_TICKET_IS_NOT_EQUAL_FOR_THE_FOLLOWING_FIELDS,
                            String.join(", ", problemFieldsOfTicket)
                    ),
                    errors
            );
            log.error(
                    format(
                            ACCOUNT_OF_TICKET_IS_NOT_EQUAL_FOR_THE_FOLLOWING_FIELDS,
                            String.join(", ", problemFieldsOfTicket)
                    )
            );
        }
    }

    private void checkAuthorExecutorsAndObserversForWeightOfRoles
            (Ticket ticket, Map<String, List<ValidationError>> errors) {
        var weightOfRoleAuthor = Roles.AUTHOR.getWeight();
        var weightOfRoleExecutor = Roles.EXECUTOR.getWeight();
        var weightOfRoleObserver = Roles.OBSERVER.getWeight();
        if (ticket.getAuthor().getRole().getWeight() < weightOfRoleAuthor) {
            addValidationErrorIntoErrors(
                    WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR,
                    format(
                            WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR_LESS_THAN_WEIGHT_OF_ROLE_AUTHOR,
                            ticket.getAuthor().getRole().getWeight(),
                            weightOfRoleAuthor
                    ),
                    errors
            );
            log.error(
                    format(
                            WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR_LESS_THAN_WEIGHT_OF_ROLE_AUTHOR,
                            ticket.getAuthor().getRole().getWeight(),
                            weightOfRoleAuthor
                    )
            );
        }

        if (ticket.getExecutors() != null && !ticket.getExecutors().isEmpty()) {
            for (User executor : ticket.getExecutors()) {
                if (executor.getRole().getWeight() < weightOfRoleExecutor) {
                    addValidationErrorIntoErrors(
                            WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS,
                            format(
                                    WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS_LESS_THAN_WEIGHT_OF_ROLE_EXECUTOR,
                                    executor.getRole().getWeight(),
                                    weightOfRoleExecutor
                            ),
                            errors
                    );
                    log.error(
                            format(
                                    WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS_LESS_THAN_WEIGHT_OF_ROLE_EXECUTOR,
                                    executor.getRole().getWeight(),
                                    weightOfRoleExecutor
                            )
                    );
                    break;
                }
            }
        }

        if (ticket.getObservers() != null && !ticket.getObservers().isEmpty()) {
            for (User observer : ticket.getObservers()) {
                if (observer.getRole().getWeight() < weightOfRoleObserver) {
                    addValidationErrorIntoErrors(
                            WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS,
                            format(
                                    WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS_LESS_THAN_WEIGHT_OF_ROLE_OBSERVER,
                                    observer.getRole().getWeight(),
                                    weightOfRoleObserver
                            ),
                            errors
                    );
                    log.error(
                            format(
                                    WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS_LESS_THAN_WEIGHT_OF_ROLE_OBSERVER,
                                    observer.getRole().getWeight(),
                                    weightOfRoleObserver
                            )
                    );
                    break;
                }
            }
        }

    }

}
