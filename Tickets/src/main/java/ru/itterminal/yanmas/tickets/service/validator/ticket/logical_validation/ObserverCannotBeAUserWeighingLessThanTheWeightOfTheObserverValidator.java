package ru.itterminal.yanmas.tickets.service.validator.ticket.logical_validation;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.tickets.model.Ticket;

import java.util.List;
import java.util.Map;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

@Component
public class ObserverCannotBeAUserWeighingLessThanTheWeightOfTheObserverValidator implements EntityValidator<Ticket> {
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS_LESS_THAN_WEIGHT_OF_ROLE_OBSERVER =
            "Weight of role (%s) into field Observers less than weight of role Observer (%s)";
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS = "Weight of role into field Observers";


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
        var weightOfRoleObserver = Roles.OBSERVER.getWeight();
        if (ticket.getObservers() != null && !ticket.getObservers().isEmpty()) {
            for (User observer : ticket.getObservers()) {
                if (observer.getRole().getWeight() < weightOfRoleObserver) {
                    addValidationErrorIntoErrors(
                            WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS,
                            format(
                                    WEIGHT_OF_ROLE_INTO_FIELD_OBSERVERS_LESS_THAN_WEIGHT_OF_ROLE_OBSERVER,
                                    observer.getRole().getWeight(),
                                    weightOfRoleObserver
                            ),
                            errors
                    );
                    break;
                }
            }
        }
    }
}
