package ru.itterminal.botdesk.tickets.service.validator;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.ifErrorsNotEmptyThrowLogicalValidationException;

import java.util.List;
import java.util.Map;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.tickets.model.Ticket;

@Slf4j
@Component
@RequiredArgsConstructor
public class TicketOperationValidator extends BasicOperationValidatorImpl<Ticket> {

    private final GroupServiceImpl groupService;
    private final UserServiceImpl userService;

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
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR = "Weight of role into field Author";
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS = "Weight of role into field Executors";
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS = "Weight of role into field Observers";
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR_LESS_THAN_WEIGHT_OF_ROLE_AUTHOR =
            "Weight of role (%s) into field Author less than weight of role Author (%s)";
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS_LESS_THAN_WEIGHT_OF_ROLE_EXECUTOR =
            "Weight of role (%s) into field Executors less than weight of role Executor (%s)";
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS_LESS_THAN_WEIGHT_OF_ROLE_OBSERVER =
            "Weight of role (%s) into field Observers less than weight of role Observer (%s)";
    public static final String
            CURRENT_USER_WITH_ROLE_AUTHOR_CAN_NOT_UPDATE_TICKET =
            "Current user with role AUTHOR can not update ticket if author of ticket is not equal current user";
    public static final String
            CURRENT_USER_WITH_ROLE_EXECUTOR_CAN_NOT_UPDATE_TICKET =
            "Current user with role EXECUTOR can not update ticket if he is not in executors of ticket";
    public static final String
            LOG_CURRENT_USER_WITH_ROLE_AUTHOR_CAN_NOT_CREATE_TICKET =
            "Current user %s with role AUTHOR can not create ticket if author of ticket %s is not equal current user";

    public static final String EMPTY_TICKET = "Empty ticket";
    public static final String LOG_EMPTY_TICKET = "Mustn't create/update ticket if subject, description and files are"
            + " empty: {}";
    public static final String MUST_NOT_CREATE_UPDATE_TICKET_IF_SUBJECT_DESCRIPTION_AND_FILES_ARE_EMPTY =
            "Mustn't create/update ticket if subject, description and files are empty";

    @Override
    public boolean beforeCreate(Ticket entity) {
        var result = super.beforeCreate(entity);
        beforeCreateUpdate(entity);
        return result;
    }

    @Override
    public boolean beforeUpdate(Ticket entity) {
        var result = super.beforeUpdate(entity);
        checkAccessForCreate(entity);
        beforeCreateUpdate(entity);
        return result;
    }

    private void beforeCreateUpdate(Ticket entity) {
        var errors = createMapForLogicalErrors();
        IsEmptySubjectDescriptionAndFiles(entity, errors);
        checkGroupsOfAuthorAndTicket(entity, errors);
        checkCurrentUserForGroupAndIsInner(entity, errors);
        checkAuthorExecutorsAndObserversForWeightOfRoles(entity, errors);
        ifErrorsNotEmptyThrowLogicalValidationException(errors);
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

    private void checkGroupsOfAuthorAndTicket(Ticket ticket, Map<String, List<ValidationError>> errors) {
        var groupFromTicket = ticket.getGroup();
        var groupFromAuthorOfTicket = ticket.getAuthor().getGroup();
        if (!groupFromAuthorOfTicket.equals(groupFromTicket)) {
            addValidationErrorIntoErrors(
                    GROUP_OF_TICKET,
                    GROUP_OF_TICKET_MUST_EQUALS_GROUP_OF_AUTHOR_OF_TICKET,
                    errors
            );
            log.error(LOG_GROUP_OF_TICKET, groupFromTicket, groupFromAuthorOfTicket);
        }
    }

    private void checkCurrentUserForGroupAndIsInner(Ticket ticket, Map<String, List<ValidationError>> errors) {
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        var groupOfCurrentUser = groupService.findByIdAndAccountId(jwtUser.getGroupId(), jwtUser.getAccountId());
        var groupFromTicket = ticket.getGroup();
        if (Boolean.FALSE.equals(groupOfCurrentUser.getIsInner()) && !groupOfCurrentUser.equals(groupFromTicket)) {
            addValidationErrorIntoErrors(
                    USER_IS_NOT_FROM_INNER_GROUP,
                    USER_FROM_NOT_INNER_GROUP_MUST_CREATE_UPDATE_TICKET_ONLY_WITH_HIS_GROUP,
                    errors
            );
            log.error(LOG_USER_FROM_NOT_INNER_GROUP, jwtUser, ticket);
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

    private void checkAccessForCreate(Ticket ticket) {
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        var currentUser = userService.findByEmail(jwtUser.getUsername()).get();
        var nameOfRoleOfCurrentUser = currentUser.getRole().getName();
        var isCurrentUserFromInnerGroup = currentUser.getGroup().getIsInner();
        var isAuthorOfTicketFromInnerGroup = ticket.getAuthor().getGroup().getIsInner();

        if (nameOfRoleOfCurrentUser.equals(Roles.ADMIN.toString())
                && Boolean.FALSE.equals(isCurrentUserFromInnerGroup)
                && Boolean.TRUE.equals(isAuthorOfTicketFromInnerGroup)) {
            throw new AccessDeniedException
                    ("Current user with role ADMIN from outer group can not create ticket if author of ticket is "
                             + "from inner group");
        }

//        if (nameOfRoleOfCurrentUser.getName().equals(Roles.AUTHOR.toString()) && !currentUser
//                .equals(ticket.getAuthor())) {
//            log.error(format(LOG_CURRENT_USER_WITH_ROLE_AUTHOR_CAN_NOT_CREATE_TICKET, currentUser, ticket));
//            throw new AccessDeniedException(CURRENT_USER_WITH_ROLE_AUTHOR_CAN_NOT_UPDATE_TICKET);
//        }
//        if (nameOfRoleOfCurrentUser.getName().equals(Roles.EXECUTOR.toString())
//                && !ticket.getExecutors().contains(currentUser)) {
//            log.error(format(""));
//            throw new AccessDeniedException(CURRENT_USER_WITH_ROLE_AUTHOR_CAN_NOT_UPDATE_TICKET);
//        }
    }

}
