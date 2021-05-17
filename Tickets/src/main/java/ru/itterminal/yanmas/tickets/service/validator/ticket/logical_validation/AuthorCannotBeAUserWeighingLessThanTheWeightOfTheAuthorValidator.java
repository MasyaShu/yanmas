package ru.itterminal.yanmas.tickets.service.validator.ticket.logical_validation;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.tickets.model.Ticket;

import java.util.List;
import java.util.Map;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

@Component
public class AuthorCannotBeAUserWeighingLessThanTheWeightOfTheAuthorValidator implements EntityValidator<Ticket> {
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR_LESS_THAN_WEIGHT_OF_ROLE_AUTHOR =
            "Weight of role (%s) into field Author less than weight of role Author (%s)";
    String WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR = "Weight of role into field Author";

    @Override
    public void logicalValidationBeforeCreate(Ticket entity, Map<String, List<ValidationError>> errors) {
        checkAuthorExecutorsAndObserversForWeightOfRoles(entity, errors);
    }

    @Override
    public void logicalValidationBeforeUpdate(Ticket entity, Map<String, List<ValidationError>> errors) {
        checkAuthorExecutorsAndObserversForWeightOfRoles(entity, errors);
    }

    private void checkAuthorExecutorsAndObserversForWeightOfRoles
            (Ticket ticket, Map<String, List<ValidationError>> errors) {
        var weightOfRoleAuthor = Roles.AUTHOR.getWeight();
        if (ticket.getAuthor().getRole().getWeight() < Roles.AUTHOR.getWeight()) {
            addValidationErrorIntoErrors(
                    WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR,
                    format(
                            WEIGHT_OF_ROLE_INTO_FIELD_AUTHOR_LESS_THAN_WEIGHT_OF_ROLE_AUTHOR,
                            ticket.getAuthor().getRole().getWeight(),
                            weightOfRoleAuthor
                    ),
                    errors
            );
        }
    }
}
