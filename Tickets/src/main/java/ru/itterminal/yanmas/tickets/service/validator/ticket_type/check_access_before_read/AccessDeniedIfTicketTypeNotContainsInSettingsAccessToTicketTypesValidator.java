package ru.itterminal.yanmas.tickets.service.validator.ticket_type.check_access_before_read;

import lombok.RequiredArgsConstructor;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.tickets.model.TicketType;
import ru.itterminal.yanmas.tickets.service.impl.SettingsAccessToTicketTypesServiceImpl;

@Component
@RequiredArgsConstructor
public class AccessDeniedIfTicketTypeNotContainsInSettingsAccessToTicketTypesValidator implements EntityValidator<TicketType> {

    public static final String ACCESS_IS_DENIED_FOR_SEARCHING_BY_PASSED_TICKET_TYPE_ID =
            "Access is denied for searching by passed groupId";

    private final SettingsAccessToTicketTypesServiceImpl settingsAccessToTicketTypesService;

    @Override
    public void checkAccessBeforeRead(TicketType entity, User currentUser) {
        var permittedTicketTypes = settingsAccessToTicketTypesService.getPermittedTicketTypes(currentUser.getId());
        if (permittedTicketTypes != null && !permittedTicketTypes.contains(entity)) {
            throw new AccessDeniedException(ACCESS_IS_DENIED_FOR_SEARCHING_BY_PASSED_TICKET_TYPE_ID);
        }
    }
}
