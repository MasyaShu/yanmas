package ru.itterminal.yanmas.tickets.service.validator.ticket.check_access_before_create;

import lombok.RequiredArgsConstructor;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.files.model.File;
import ru.itterminal.yanmas.tickets.model.Ticket;

@Component
@RequiredArgsConstructor
public class AccessDeniedIfFileWasCreatedByAnotherUserValidator implements EntityValidator<Ticket> {
    public static final String
            ACCESS_DENIED_BECAUSE_FILE_WAS_CREATED_BY_ANOTHER_USER_YOU_CANNOT_USE_IT_FOR_CREATE_THIS_TICKET =
            "Access denied, because file was created by another user, you cannot use it for create this ticket";

    @Override
    public void checkAccessBeforeCreate(Ticket entity, User currentUser) {
        if (entity.getFiles() != null && !entity.getFiles().isEmpty()) {
            for (File file : entity.getFiles()) {
                if (!file.getAuthorId().equals(currentUser.getId())) {
                    throw new AccessDeniedException(
                            ACCESS_DENIED_BECAUSE_FILE_WAS_CREATED_BY_ANOTHER_USER_YOU_CANNOT_USE_IT_FOR_CREATE_THIS_TICKET);
                }
            }
        }
    }
}
