package ru.itterminal.yanmas.tickets.service.validator;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.yanmas.files.model.File;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.service.impl.SettingsAccessToTicketTypesServiceImpl;

import java.util.List;
import java.util.Map;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.*;
import static ru.itterminal.yanmas.tickets.service.validator.TicketSettingOperationValidator.ACCESS_DENIED_BECAUSE_CURRENT_USER_HAS_NOT_PERMIT_TO_TICKET_TYPE;

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
    public static final String ACCESS_DENIED_BECAUSE_FILE_WAS_CREATED_BY_ANOTHER_USER_YOU_CANNOT_USE_IT_FOR_CREATE_THIS_TICKET =
            "Access denied, because file was created by another user, you cannot use it for create this ticket";
    public static final String FILE_ALREADY_HAS_A_LINK_TO_ANOTHER_ENTITY = "File already has a link to another entity";
    public static final String FILE_IS_NOT_YET_UPLOADED = "File is not yet uploaded";
    public static final String INVALID_TICKET = "Invalid ticket";
    public static final String INVALID_TICKET_BECAUSE_AUTHOR_HAS_NOT_ACCESS_TO_TICKET_TYPE =
            "Invalid ticket, because author has not access to ticket type";


    private final SettingsAccessToTicketTypesServiceImpl settingsAccessToTicketTypesService;

    public void checkAccessBeforeCreate(Ticket entity, User currentUser) {
        checkAccessForCreateAndUpdate(entity, currentUser);
        chekFilesForAccessBeforeCreate(entity, currentUser);
    }

    @Override
    public boolean logicalValidationBeforeCreate(Ticket entity) {
        var result = super.logicalValidationBeforeCreate(entity);
        var errors = createMapForLogicalErrors();
        isEmptySubjectDescriptionAndFiles(entity, errors);
        checkAuthorExecutorsAndObserversForWeightOfRoles(entity, errors);
        isAuthorOfTicketHavePermitToTicketType(entity, errors);
        chekFilesForLogicalValidationBeforeCreate(entity, errors);
        ifErrorsNotEmptyThrowLogicalValidationException(errors);
        return result;
    }

    private void chekFilesForLogicalValidationBeforeCreate(Ticket entity, Map<String, List<ValidationError>> errors) {
        if (entity.getFiles() != null && !entity.getFiles().isEmpty()) {
            for (File file : entity.getFiles()) {
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

    private void chekFilesForAccessBeforeCreate(Ticket entity, User currentUser) {
        if (entity.getFiles() != null && !entity.getFiles().isEmpty()) {
            for (File file : entity.getFiles()) {
                if (!file.getAuthorId().equals(currentUser.getId())) {
                    throw new AccessDeniedException(ACCESS_DENIED_BECAUSE_FILE_WAS_CREATED_BY_ANOTHER_USER_YOU_CANNOT_USE_IT_FOR_CREATE_THIS_TICKET);
                }
            }
        }
    }

    public void checkAccessBeforeUpdate(Ticket entity, User currentUser) {
        checkAccessForCreateAndUpdate(entity, currentUser);
    }

    @Override
    public boolean logicalValidationBeforeUpdate(Ticket entity) {
        var result = super.logicalValidationBeforeUpdate(entity);
        var errors = createMapForLogicalErrors();
        isEmptySubjectDescriptionAndFiles(entity, errors);
        checkAuthorExecutorsAndObserversForWeightOfRoles(entity, errors);
        isAuthorOfTicketHavePermitToTicketType(entity, errors);
        ifErrorsNotEmptyThrowLogicalValidationException(errors);
        return result;
    }

    private void isAuthorOfTicketHavePermitToTicketType(Ticket ticket, Map<String, List<ValidationError>> errors) {
        if (ticket.getTicketType() != null && ticket.getAuthor() != null) {
            var authorId = ticket.getAuthor().getId();
            var ticketTypeId = ticket.getTicketType().getId();
            if (!settingsAccessToTicketTypesService.isPermittedTicketType(ticketTypeId, authorId)) {
                addValidationErrorIntoErrors(
                        INVALID_TICKET, INVALID_TICKET_BECAUSE_AUTHOR_HAS_NOT_ACCESS_TO_TICKET_TYPE,
                        errors
                );
                log.trace(INVALID_TICKET_BECAUSE_AUTHOR_HAS_NOT_ACCESS_TO_TICKET_TYPE);
            }
        }
    }


    private void isEmptySubjectDescriptionAndFiles(Ticket ticket, Map<String, List<ValidationError>> errors) {
        if ((ticket.getDescription() == null || ticket.getDescription().isEmpty())
                && (ticket.getSubject() == null || ticket.getSubject().isEmpty())
                && (ticket.getFiles() == null || ticket.getFiles().isEmpty())) {
            addValidationErrorIntoErrors(
                    EMPTY_TICKET,
                    MUST_NOT_CREATE_UPDATE_TICKET_IF_SUBJECT_DESCRIPTION_AND_FILES_ARE_EMPTY,
                    errors
            );
            log.trace(LOG_EMPTY_TICKET, ticket);
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
            log.trace(
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
                    log.trace(
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
                    log.trace(
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
    public boolean checkAccessForCreateAndUpdate(Ticket ticket, User currentUser) { //NOSONAR
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

        // checkAccessForCreateAndUpdate_ShouldGetAccessDeniedException_whenCurrentUserHasNotPermitToTicketType
        if (ticket.getTicketType() != null) {
            var ticketTypeId = ticket.getTicketType().getId();
            var currentUserId = currentUser.getId();
            if (!settingsAccessToTicketTypesService.isPermittedTicketType(ticketTypeId, currentUserId)) {
                throw new AccessDeniedException(ACCESS_DENIED_BECAUSE_CURRENT_USER_HAS_NOT_PERMIT_TO_TICKET_TYPE);
            }
        }

        return true;
    }

    @SuppressWarnings("DuplicatedCode")
    public void checkAccessBeforeRead(Ticket ticket, User currentUser) { //NOSONAR
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

        // checkAccessForRead_ShouldGetAccessDeniedException_whenCurrentUserHasNotPermitToTicketType
        if (ticket.getTicketType() != null) {
            var ticketTypeId = ticket.getTicketType().getId();
            var currentUserId = currentUser.getId();
            if (!settingsAccessToTicketTypesService.isPermittedTicketType(ticketTypeId, currentUserId)) {
                throw new AccessDeniedException(ACCESS_DENIED_BECAUSE_CURRENT_USER_HAS_NOT_PERMIT_TO_TICKET_TYPE);
            }
        }
    }

}
