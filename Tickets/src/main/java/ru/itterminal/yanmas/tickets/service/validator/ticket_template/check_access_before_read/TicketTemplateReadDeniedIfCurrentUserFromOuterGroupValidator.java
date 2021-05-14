package ru.itterminal.yanmas.tickets.service.validator.ticket_template.check_access_before_read;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.tickets.model.TicketTemplate;

@Component

public class TicketTemplateReadDeniedIfCurrentUserFromOuterGroupValidator implements EntityValidator<TicketTemplate> {
    @Override
    public void checkAccessBeforeRead(User currentUser) {
        throwAccessDeniedExceptionIfCurrentUserFromOuterGroup(currentUser);
    }
}
