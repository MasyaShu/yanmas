package ru.itterminal.yanmas.tickets.service.validator.ticket_setting.check_access_before_update;

import lombok.RequiredArgsConstructor;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.tickets.model.TicketSetting;
import ru.itterminal.yanmas.tickets.service.impl.SettingsAccessToTicketTypesServiceImpl;

@Component
@RequiredArgsConstructor
public class NotAllowedUpdateFromCurrentUserIfTicketTypeIsNotPermitted implements EntityValidator<TicketSetting> {

    public static final String ACCESS_DENIED_BECAUSE_CURRENT_USER_HAS_NOT_PERMIT_TO_TICKET_TYPE =
            "Access denied, because current user has not permit to ticket type";

    private final SettingsAccessToTicketTypesServiceImpl settingsAccessToTicketTypesService;

    @Override
    public void checkAccessBeforeUpdate(TicketSetting entity, User currentUser) {
        if (entity.getTicketTypeForNew() != null
                && !settingsAccessToTicketTypesService.isPermittedTicketType(entity.getTicketTypeForNew().getId(), currentUser.getId())) {
            throw new AccessDeniedException(ACCESS_DENIED_BECAUSE_CURRENT_USER_HAS_NOT_PERMIT_TO_TICKET_TYPE);
        }
    }
}
