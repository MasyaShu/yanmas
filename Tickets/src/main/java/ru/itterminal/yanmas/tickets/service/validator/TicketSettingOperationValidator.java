package ru.itterminal.yanmas.tickets.service.validator;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.yanmas.security.jwt.JwtUser;
import ru.itterminal.yanmas.tickets.model.TicketSetting;

import java.util.UUID;

@Component
public class TicketSettingOperationValidator extends BasicOperationValidatorImpl<TicketSetting> {

    public static final String A_USER_CANNOT_GET_SETTING_OR_PREDEFINED_VALUES_FOR_TICKET =
            "A user cannot get setting or predefined values for ticket if his group is not equal group of author from request";

    public void checkAccessForGetSettingOrPredefinedValuesForTicket(UUID groupIdOfAuthor) {
        var jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (!jwtUser.getGroupId().equals(groupIdOfAuthor)) {
            throw new AccessDeniedException(A_USER_CANNOT_GET_SETTING_OR_PREDEFINED_VALUES_FOR_TICKET);
        }
    }
}
