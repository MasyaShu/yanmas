package ru.itterminal.yanmas.tickets.service.validator;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.BasicOperationValidatorWithCurrentUserImpl;
import ru.itterminal.yanmas.files.model.File;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.service.impl.SettingsAccessToTicketTypesServiceImpl;
import ru.itterminal.yanmas.tickets.service.impl.TicketServiceImpl;

import static java.lang.String.format;
import static ru.itterminal.yanmas.tickets.service.validator.ticket_setting.check_access_before_create.NotAllowedCreateFromCurrentUserIfTicketTypeIsNotPermitted.ACCESS_DENIED_BECAUSE_CURRENT_USER_HAS_NOT_PERMIT_TO_TICKET_TYPE;

@Slf4j
@Component
@RequiredArgsConstructor
public class TicketOperationValidator extends BasicOperationValidatorWithCurrentUserImpl<Ticket> {

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

    public static final String
            ACCESS_DENIED_BECAUSE_FILE_WAS_CREATED_BY_ANOTHER_USER_YOU_CANNOT_USE_IT_FOR_CREATE_THIS_TICKET =
            "Access denied, because file was created by another user, you cannot use it for create this ticket";


    private final SettingsAccessToTicketTypesServiceImpl settingsAccessToTicketTypesService;

    public void checkAccessBeforeCreate(Ticket entity, User currentUser) {
        checkAccessForCreateAndUpdate(entity, currentUser);
        chekFilesForAccessBeforeCreate(entity, currentUser);
    }


    private void chekFilesForAccessBeforeCreate(Ticket entity, User currentUser) {
        if (entity.getFiles() != null && !entity.getFiles().isEmpty()) {
            for (File file : entity.getFiles()) {
                if (!file.getAuthorId().equals(currentUser.getId())) {
                    throw new AccessDeniedException(
                            ACCESS_DENIED_BECAUSE_FILE_WAS_CREATED_BY_ANOTHER_USER_YOU_CANNOT_USE_IT_FOR_CREATE_THIS_TICKET);
                }
            }
        }
    }

    public void checkAccessBeforeUpdate(Ticket entity, User currentUser) {
        checkAccessForCreateAndUpdate(entity, currentUser);
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

}
