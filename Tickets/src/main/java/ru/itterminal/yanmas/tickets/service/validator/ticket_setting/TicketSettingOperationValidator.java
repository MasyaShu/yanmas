package ru.itterminal.yanmas.tickets.service.validator.ticket_setting;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.yanmas.tickets.model.TicketSetting;

@Component
public class TicketSettingOperationValidator extends BasicOperationValidatorImpl<TicketSetting> {

    public static final String CURRENT_USER_FROM_OUTER_GROUP_CANNOT_GET_SETTING_OR_PREDEFINED_VALUES_FOR_TICKET_IF_HIS_GROUP_IS_NOT_EQUAL_GROUP_OF_AUTHOR_FROM_REQUEST =
            "Current user from outer group cannot get setting or predefined values for ticket if his group is not equal group of author from request";

    public void checkAccessForGetSettingOrPredefinedValuesForTicket(User currentUser, User foundUser) {
        if (Boolean.FALSE.equals(currentUser.getGroup().getIsInner()) &&
                !currentUser.getGroup().getId().equals(foundUser.getGroup().getId())) {
            throw new AccessDeniedException(CURRENT_USER_FROM_OUTER_GROUP_CANNOT_GET_SETTING_OR_PREDEFINED_VALUES_FOR_TICKET_IF_HIS_GROUP_IS_NOT_EQUAL_GROUP_OF_AUTHOR_FROM_REQUEST);
        }
    }
}
