package ru.itterminal.yanmas.tickets.service.validator;

import lombok.Builder;
import lombok.Getter;

@SuppressWarnings("UnusedAssignment")
@Builder
@Getter
public class TestDataCrudTicketPermission {
    int id;
    String nameOfRoleOfCurrentUser;
    boolean isCurrentUserFromInnerGroup;
    boolean isAuthorOfTicketFromInnerGroup;
    boolean isCurrentUserEqualAuthorOfTicket = false;
    boolean isGroupOfCurrentUserEqualGroupOfAuthorOfTicket = false;
    boolean isTicketsObserversContainsCurrentUser = true;
    boolean isTicketsExecutorContainsCurrentUser = true;
    String exceptionMessage;
}
