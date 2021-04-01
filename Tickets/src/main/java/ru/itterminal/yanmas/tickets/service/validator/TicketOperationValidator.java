package ru.itterminal.yanmas.tickets.service.validator;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.ifErrorsNotEmptyThrowLogicalValidationException;

import java.util.List;
import java.util.Map;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.yanmas.files.model.File;
import ru.itterminal.yanmas.security.jwt.JwtUser;
import ru.itterminal.yanmas.tickets.model.Ticket;

@Slf4j
@Component
@RequiredArgsConstructor
public class TicketOperationValidator extends BasicOperationValidatorImpl<Ticket> {

    public static final String
            CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP =
            "Current user with role ADMIN from outer group can not create/update ticket if author of ticket is from inner group";
    public static final String
            LOG_CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP =
            "Current user (%s) with role ADMIN from outer group can not create/update ticket (%s) if author of ticket is from inner group";
    public static final String
            LOG_CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP =
            "Current user (%s) with role ADMIN from outer group can not create/update ticket (%s) if ticket is from another group";
    public static final String
            CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP =
            "Current user with role ADMIN from outer group can not create/update ticket if ticket is from another group";
    public static final String
            LOG_CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP =
            "Current user (%s) with role EXECUTOR from outer group can not create/update ticket (%s) if author of ticket is from inner group";
    public static final String
            CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP =
            "Current user with role EXECUTOR from outer group can not create/update ticket if author of ticket is from inner group";
    public static final String
            LOG_CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP =
            "Current user (%s) with role EXECUTOR from outer group can not create/update ticket (%s) if ticket is from another group";
    public static final String
            CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP =
            "Current user with role EXECUTOR from outer group can not create/update ticket if ticket is from another group";
    public static final String
            LOG_CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_NOT_CURRENT_USER =
            "Current user (%s) with role AUTHOR from inner group can not create/update ticket (%s) if author of ticket is not current user";
    public static final String
            CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_NOT_CURRENT_USER =
            "Current user with role AUTHOR from inner group can not create/update ticket if author of ticket is not current user";
    public static final String
            LOG_CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_NOT_CURRENT_USER =
            "Current user (%s) with role AUTHOR from outer group can not create/update ticket (%s) if author of ticket is not current user";
    public static final String
            CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_NOT_CURRENT_USER =
            "Current user with role AUTHOR from outer group can not create/update ticket if author of ticket is not current user";
    public static final String
            LOG_CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP =
            "Current user (%s) with role AUTHOR from outer group can not create/update ticket (%s) if author of ticket is from inner group";
    public static final String
            CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP =
            "Current user with role AUTHOR from outer group can not create/update ticket if author of ticket is from inner group";
    public static final String
            LOG_CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_OUTER_GROUP =
            "Current user (%s) with role AUTHOR from inner group can not create/update ticket (%s) if author of ticket is from outer group";
    public static final String
            CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_OUTER_GROUP =
            "Current user with role AUTHOR from inner group can not create/update ticket if author of ticket is from outer group";
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR = "Weight of role into field Author";
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS = "Weight of role into field Executors";
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS = "Weight of role into field Observers";
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR_LESS_THAN_WEIGHT_OF_ROLE_AUTHOR =
            "Weight of role (%s) into field Author less than weight of role Author (%s)";
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS_LESS_THAN_WEIGHT_OF_ROLE_EXECUTOR =
            "Weight of role (%s) into field Executors less than weight of role Executor (%s)";
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS_LESS_THAN_WEIGHT_OF_ROLE_OBSERVER =
            "Weight of role (%s) into field Observers less than weight of role Observer (%s)";
    public static final String EMPTY_TICKET = "Empty ticket";
    public static final String LOG_EMPTY_TICKET = "Mustn't create/update ticket if subject, description and files are"
            + " empty: {}";
    public static final String MUST_NOT_CREATE_UPDATE_TICKET_IF_SUBJECT_DESCRIPTION_AND_FILES_ARE_EMPTY =
            "Mustn't create/update ticket if subject, description and files are empty";
    public static final String
            LOG_CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP =
            "Current user (%s) with role ADMIN from outer group can not read ticket (%s) if author of ticket is from inner group";
    public static final String
            CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP =
            "Current user with role ADMIN from outer group can not read ticket if author of ticket is from inner group";
    public static final String
            LOG_CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP =
            "Current user (%s) with role ADMIN from outer group can not read ticket (%s) if ticket is from another group";
    public static final String
            CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP =
            "Current user with role ADMIN from outer group can not read ticket if ticket is from another group";
    public static final String
            LOG_CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP =
            "Current user (%s) with role EXECUTOR from outer group can not read ticket (%s) if author of ticket is from inner group";
    public static final String
            CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP =
            "Current user with role EXECUTOR from outer group can not read ticket if author of ticket is from inner group";
    public static final String
            LOG_CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP =
            "Current user (%s) with role EXECUTOR from outer group can not read ticket (%s) if ticket is from another group";
    public static final String
            CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP =
            "Current user with role EXECUTOR from outer group can not read ticket if ticket is from another group";
    public static final String
            LOG_CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_OUTER_GROUP =
            "Current user (%s) with role AUTHOR from inner group can not read ticket (%s) if author of ticket is from outer group";
    public static final String
            CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_OUTER_GROUP =
            "Current user with role AUTHOR from inner group can not read ticket if author of ticket is from outer group";
    public static final String
            LOG_CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_AS_A_AUTHOR_AND_HAS_NOT_IN_OBSERVERS =
            "Current user (%s) with role AUTHOR from inner group can not read ticket (%s) if ticket has not current user as a author and has not in observers";
    public static final String
            CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_AS_A_AUTHOR_AND_HAS_NOT_IN_OBSERVERS =
            "Current user with role AUTHOR from inner group can not read ticket if ticket has not current user as a author and has not in observers";
    public static final String
            LOG_CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP =
            "Current user (%s) with role AUTHOR from outer group can not read ticket (%s) if author of ticket is from inner group";
    public static final String
            CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP =
            "Current user with role AUTHOR from outer group can not read ticket if author of ticket is from inner group";
    public static final String
            LOG_CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_AS_A_AUTHOR_AND_HAS_NOT_CURRENT_USER_IN_OBSERVERS =
            "Current user (%s) with role AUTHOR from outer group can not read ticket (%s) if ticket has not current user as a author and has not current user in observers";
    public static final String
            CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_AS_A_AUTHOR_AND_HAS_NOT_IN_OBSERVERS =
            "Current user with role AUTHOR from outer group can not read ticket if ticket has not current user as a author and has not in observers";
    public static final String
            LOG_CURRENT_USER_WITH_ROLE_OBSERVER_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_IN_OBSERVERS =
            "Current user (%s) with role OBSERVER can not read ticket (%s) if ticket has not current user in observers";
    public static final String
            CURRENT_USER_WITH_ROLE_OBSERVER_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_IN_OBSERVERS =
            "Current user with role OBSERVER can not read ticket if ticket has not current user in observers";
    public static final String FILE_IS_INVALID = "File is invalid";
    public static final String FILE_WAS_CREATED_BY_ANOTHER_USER_YOU_CANNOT_USE_IT_FOR_CREATE_THIS_TICKET =
            "File was created by another user, you cannot use it for create this ticket";
    public static final String FILE_ALREADY_HAS_A_LINK_TO_ANOTHER_ENTITY = "File already has a link to another entity";
    public static final String FILE_IS_NOT_YET_UPLOADED = "File is not yet uploaded";

    private final UserServiceImpl userService;

    @Override
    public void checkAccessBeforeCreate(Ticket entity) {
        checkAccessForCreateAndUpdate(entity);
    }

    @Override
    public boolean logicalValidationBeforeCreate(Ticket entity) {
        var result = super.logicalValidationBeforeCreate(entity);
        var errors = createMapForLogicalErrors();
        IsEmptySubjectDescriptionAndFiles(entity, errors);
        checkAuthorExecutorsAndObserversForWeightOfRoles(entity, errors);
        chekFiles(entity, errors);
        ifErrorsNotEmptyThrowLogicalValidationException(errors);
        return result;
    }

    private void chekFiles(Ticket entity, Map<String, List<ValidationError>> errors) {
        if (entity.getFiles() != null && !entity.getFiles().isEmpty()) {
            JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
            for (File file : entity.getFiles()) {
                if (!file.getAuthorId().equals(jwtUser.getId())) {
                    addValidationErrorIntoErrors(
                            FILE_IS_INVALID,
                            FILE_WAS_CREATED_BY_ANOTHER_USER_YOU_CANNOT_USE_IT_FOR_CREATE_THIS_TICKET,
                            errors
                    );
                }
                if (file.getEntityId() != null) {
                    addValidationErrorIntoErrors(
                            FILE_IS_INVALID,
                            FILE_ALREADY_HAS_A_LINK_TO_ANOTHER_ENTITY,
                            errors
                    );
                }
                if (Boolean.FALSE.equals(file.getIsUploaded())) {
                    addValidationErrorIntoErrors(
                            FILE_IS_INVALID,
                            FILE_IS_NOT_YET_UPLOADED,
                            errors
                    );
                }
            }
        }
    }

    @Override
    public void checkAccessBeforeUpdate(Ticket entity) {
        checkAccessForCreateAndUpdate(entity);
    }

    @Override
    public boolean logicalValidationBeforeUpdate(Ticket entity) {
        var result = super.logicalValidationBeforeUpdate(entity);
        var errors = createMapForLogicalErrors();
        IsEmptySubjectDescriptionAndFiles(entity, errors);
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

    @SuppressWarnings("DuplicatedCode")
    public boolean checkAccessForCreateAndUpdate(Ticket ticket) {
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        var currentUser = userService.findByEmail(jwtUser.getUsername());
        var nameOfRoleOfCurrentUser = currentUser.getRole().getName();
        var isCurrentUserFromInnerGroup = currentUser.getGroup().getIsInner();
        var isAuthorOfTicketFromInnerGroup = ticket.getAuthor().getGroup().getIsInner();
        var groupOfCurrentUser = currentUser.getGroup();
        var groupOfAuthorOfTicket = ticket.getAuthor().getGroup();
        var authorOfTicket = ticket.getAuthor();

        // test data id = 1
        if (nameOfRoleOfCurrentUser.equals(Roles.ADMIN.toString())
                && Boolean.FALSE.equals(isCurrentUserFromInnerGroup)
                && Boolean.TRUE.equals(isAuthorOfTicketFromInnerGroup)) {
            log.trace(
                    format(
                            LOG_CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP,
                            currentUser, ticket
                    ));
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP);
        }

        // test data id = 2
        if (nameOfRoleOfCurrentUser.equals(Roles.ADMIN.toString())
                && Boolean.FALSE.equals(isCurrentUserFromInnerGroup)
                && Boolean.FALSE.equals(isAuthorOfTicketFromInnerGroup)
                && !groupOfCurrentUser.equals(groupOfAuthorOfTicket)) {
            log.trace(
                    format(
                            LOG_CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP,
                            currentUser, ticket
                    ));
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP);
        }

        // test data id = 3
        if (nameOfRoleOfCurrentUser.equals(Roles.EXECUTOR.toString())
                && Boolean.FALSE.equals(isCurrentUserFromInnerGroup)
                && Boolean.TRUE.equals(isAuthorOfTicketFromInnerGroup)) {
            log.trace(
                    format(
                            LOG_CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP,
                            currentUser, ticket
                    ));
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP);
        }

        // test data id = 4
        if (nameOfRoleOfCurrentUser.equals(Roles.EXECUTOR.toString())
                && Boolean.FALSE.equals(isCurrentUserFromInnerGroup)
                && Boolean.FALSE.equals(isAuthorOfTicketFromInnerGroup)
                && !groupOfCurrentUser.equals(groupOfAuthorOfTicket)) {
            log.trace(
                    format(
                            LOG_CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP,
                            currentUser, ticket
                    ));
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP);
        }

        // test data id = 5
        if (nameOfRoleOfCurrentUser.equals(Roles.AUTHOR.toString())
                && Boolean.TRUE.equals(isCurrentUserFromInnerGroup)
                && Boolean.TRUE.equals(isAuthorOfTicketFromInnerGroup)
                && !authorOfTicket.equals(currentUser)) {
            log.trace(
                    format(
                            LOG_CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_NOT_CURRENT_USER,
                            currentUser, ticket
                    ));
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_NOT_CURRENT_USER);
        }

        // test data id = 6
        if (nameOfRoleOfCurrentUser.equals(Roles.AUTHOR.toString())
                && Boolean.FALSE.equals(isCurrentUserFromInnerGroup)
                && Boolean.FALSE.equals(isAuthorOfTicketFromInnerGroup)
                && !authorOfTicket.equals(currentUser)) {
            log.trace(
                    format(
                            LOG_CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_NOT_CURRENT_USER,
                            currentUser, ticket
                    ));
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_NOT_CURRENT_USER);
        }

        // test data id = 7
        if (nameOfRoleOfCurrentUser.equals(Roles.AUTHOR.toString())
                && Boolean.FALSE.equals(isCurrentUserFromInnerGroup)
                && Boolean.TRUE.equals(isAuthorOfTicketFromInnerGroup)) {
            log.trace(
                    format(
                            LOG_CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP,
                            currentUser, ticket
                    ));
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP);
        }

        // test data id = 8
        if (nameOfRoleOfCurrentUser.equals(Roles.AUTHOR.toString())
                && Boolean.TRUE.equals(isCurrentUserFromInnerGroup)
                && Boolean.FALSE.equals(isAuthorOfTicketFromInnerGroup)) {
            log.trace(
                    format(
                            LOG_CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_OUTER_GROUP,
                            currentUser, ticket
                    ));
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_CREATE_UPDATE_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_OUTER_GROUP);
        }
        return true;
    }

    @SuppressWarnings("DuplicatedCode")
    @Override
    public void checkAccessBeforeRead(Ticket ticket) {
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        var currentUser = userService.findByEmail(jwtUser.getUsername());
        var nameOfRoleOfCurrentUser = currentUser.getRole().getName();
        var isCurrentUserFromInnerGroup = currentUser.getGroup().getIsInner();
        var isAuthorOfTicketFromInnerGroup = ticket.getAuthor().getGroup().getIsInner();
        var groupOfCurrentUser = currentUser.getGroup();
        var groupOfAuthorOfTicket = ticket.getAuthor().getGroup();
        var authorOfTicket = ticket.getAuthor();

        // test data id = 1
        if (nameOfRoleOfCurrentUser.equals(Roles.ADMIN.toString())
                && Boolean.FALSE.equals(isCurrentUserFromInnerGroup)
                && Boolean.TRUE.equals(isAuthorOfTicketFromInnerGroup)) {
            log.trace(format(
                    LOG_CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP,
                    currentUser, ticket
            ));
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP);
        }

        // test data id = 2
        if (nameOfRoleOfCurrentUser.equals(Roles.ADMIN.toString())
                && Boolean.FALSE.equals(isCurrentUserFromInnerGroup)
                && Boolean.FALSE.equals(isAuthorOfTicketFromInnerGroup)
                && !groupOfCurrentUser.equals(groupOfAuthorOfTicket)) {
            log.trace(format(
                    LOG_CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP,
                    currentUser, ticket
            ));
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_ADMIN_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP);
        }

        // test data id = 3
        if (nameOfRoleOfCurrentUser.equals(Roles.EXECUTOR.toString())
                && Boolean.FALSE.equals(isCurrentUserFromInnerGroup)
                && Boolean.TRUE.equals(isAuthorOfTicketFromInnerGroup)) {
            log.trace(
                    format(
                            LOG_CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP,
                            currentUser, ticket
                    ));
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP);
        }

        // test data id = 4
        if (nameOfRoleOfCurrentUser.equals(Roles.EXECUTOR.toString())
                && Boolean.FALSE.equals(isCurrentUserFromInnerGroup)
                && Boolean.FALSE.equals(isAuthorOfTicketFromInnerGroup)
                && !groupOfCurrentUser.equals(groupOfAuthorOfTicket)) {
            log.trace(
                    format(
                            LOG_CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP,
                            currentUser, ticket
                    ));
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_EXECUTOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_IS_FROM_ANOTHER_GROUP);
        }

        // test data id = 6
        if (nameOfRoleOfCurrentUser.equals(Roles.AUTHOR.toString())
                && Boolean.TRUE.equals(isCurrentUserFromInnerGroup)
                && Boolean.FALSE.equals(isAuthorOfTicketFromInnerGroup)) {
            log.trace(
                    format(
                            LOG_CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_OUTER_GROUP,
                            currentUser, ticket
                    ));
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_OUTER_GROUP);
        }

        // test data id = 7
        if (nameOfRoleOfCurrentUser.equals(Roles.AUTHOR.toString())
                && Boolean.TRUE.equals(isCurrentUserFromInnerGroup)
                && !currentUser.equals(authorOfTicket)
                && (ticket.getObservers() == null || !ticket.getObservers().contains(currentUser))) {
            log.trace(
                    format(
                            LOG_CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_AS_A_AUTHOR_AND_HAS_NOT_IN_OBSERVERS,
                            currentUser, ticket
                    ));
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_AUTHOR_FROM_INNER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_AS_A_AUTHOR_AND_HAS_NOT_IN_OBSERVERS);
        }

        // test data id = 8
        if (nameOfRoleOfCurrentUser.equals(Roles.AUTHOR.toString())
                && Boolean.FALSE.equals(isCurrentUserFromInnerGroup)
                && Boolean.TRUE.equals(isAuthorOfTicketFromInnerGroup)) {
            log.trace(
                    format(
                            LOG_CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP,
                            currentUser, ticket
                    ));
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_AUTHOR_OF_TICKET_IS_FROM_INNER_GROUP);
        }

        // test data id = 9
        if (nameOfRoleOfCurrentUser.equals(Roles.AUTHOR.toString())
                && Boolean.FALSE.equals(isCurrentUserFromInnerGroup)
                && !currentUser.equals(authorOfTicket)
                && (ticket.getObservers() == null || !ticket.getObservers().contains(currentUser))) {
            log.trace(
                    format(
                            LOG_CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_AS_A_AUTHOR_AND_HAS_NOT_CURRENT_USER_IN_OBSERVERS,
                            currentUser, ticket
                    ));
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_AUTHOR_FROM_OUTER_GROUP_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_AS_A_AUTHOR_AND_HAS_NOT_IN_OBSERVERS);
        }

        // test data id = 10
        if (nameOfRoleOfCurrentUser.equals(Roles.OBSERVER.toString())
                && (ticket.getObservers() == null || !ticket.getObservers().contains(currentUser))) {
            log.trace(
                    format(
                            LOG_CURRENT_USER_WITH_ROLE_OBSERVER_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_IN_OBSERVERS,
                            currentUser, ticket
                    ));
            throw new AccessDeniedException
                    (CURRENT_USER_WITH_ROLE_OBSERVER_CAN_NOT_READ_TICKET_IF_TICKET_HAS_NOT_CURRENT_USER_IN_OBSERVERS);
        }
    }

}
