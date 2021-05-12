package ru.itterminal.yanmas.aau.service.validator.user.logical_validation_before_update;

import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

import java.util.List;
import java.util.Map;

import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.integration.across_modules.RequestsFromModuleAccountAndUsers;

@Component
@RequiredArgsConstructor
public class NotAllowedChangeGroupOfUserIfUserWasUsedInTicketsValidator implements EntityValidator<User> {

    private final UserServiceImpl service;
    private final ApplicationContext appContext;

    public static final String TICKET_SERVICE_IMPL = "ticketServiceImpl";
    public static final String CHANGE_USER_GROUP = "Change user group";
    public static final String CANNOT_CHANGE_THE_GROUP = "The user cannot change the group, because he is present in tickets";

    @Override
    public void logicalValidationBeforeUpdate(User entity,
                                              Map<String, List<ValidationError>> errors) {
        var userFromDatabase = service.findByIdAndAccountId(entity.getId(), entity.getAccount().getId());
        var groupIdOfUserFromDatabase = userFromDatabase.getGroup().getId();
        var groupIdOfUserFromEntity = entity.getGroup().getId();
        if (!groupIdOfUserFromDatabase.equals(groupIdOfUserFromEntity)) {
            var ticketService = (RequestsFromModuleAccountAndUsers) appContext.getBean(TICKET_SERVICE_IMPL);
            var countTicketWithUser = ticketService.countEntityWithUser(entity.getId());
            if (countTicketWithUser != 0) {
                addValidationErrorIntoErrors(
                        CHANGE_USER_GROUP,
                        CANNOT_CHANGE_THE_GROUP,
                        errors
                );
            }
        }
    }
}
