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
public class ExecutorCannotBeAUserWeighingLessThanTheWeightOfTheExecutorValidator implements EntityValidator<Ticket> {
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS_LESS_THAN_WEIGHT_OF_ROLE_EXECUTOR =
            "Weight of role (%s) into field Executors less than weight of role Executor (%s)";
    public static final String WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS = "Weight of role into field Executors";


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
        var weightOfRoleExecutor = Roles.EXECUTOR.getWeight();
        if (ticket.getExecutors() != null && !ticket.getExecutors().isEmpty()) {
            for (User executor : ticket.getExecutors()) {
                if (executor.getRole().getWeight() < weightOfRoleExecutor) {
                    addValidationErrorIntoErrors(
                            WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS,
                            format(
                                    WEIGHT_OF_ROLE_INTO_FIELD_EXECUTORS_LESS_THAN_WEIGHT_OF_ROLE_EXECUTOR,
                                    executor.getRole().getWeight(),
                                    weightOfRoleExecutor
                            ),
                            errors
                    );
                    break;
                }
            }
        }

    }
}
