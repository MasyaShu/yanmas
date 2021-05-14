package ru.itterminal.yanmas.tickets.service.validator.ticket_setting;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.yanmas.tickets.model.TicketSetting;

@Component
public class TicketSettingOperationValidator extends BasicOperationValidatorImpl<TicketSetting> {

    public static final String A_USER_CANNOT_GET_SETTING_OR_PREDEFINED_VALUES_FOR_TICKET =
            "A user cannot get setting or predefined values for ticket if his group is not equal group of author from request";

    public void checkAccessForGetSettingOrPredefinedValuesForTicket(User currentUSer, User foundUser) {
        if (!currentUSer.getGroup().getId().equals(foundUser.getGroup().getId())) {
            throw new AccessDeniedException(A_USER_CANNOT_GET_SETTING_OR_PREDEFINED_VALUES_FOR_TICKET);
        }
    }
}
