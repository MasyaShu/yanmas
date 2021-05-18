package ru.itterminal.yanmas.tickets.service.validator.ticket.logical_validation;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.tickets.model.Ticket;

import java.util.List;
import java.util.Map;

import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

@Component
public class ExecutorFromOuterGroupCanBeIfHisGroupEqualsGroupOfTicketValidator implements EntityValidator<Ticket> {
    public static final String EXECUTOR_FROM_OUTER_GROUP_CAN_BE_IF_HIS_GROUP_EQUALS_GROUP_OF_TICKET =
            "Executor from outer group can be if his group equals group of ticket";
    public static final String EXECUTOR_IS_INVALID = "Executor is invalid";

    @Override
    public void logicalValidationBeforeCreate(Ticket entity, Map<String, List<ValidationError>> errors) {
        checkObserversFromOuterGroupHisEqualsGroupOfTicket(entity, errors);
    }

    @Override
    public void logicalValidationBeforeUpdate(Ticket entity, Map<String, List<ValidationError>> errors) {
        checkObserversFromOuterGroupHisEqualsGroupOfTicket(entity, errors);
    }

    private void checkObserversFromOuterGroupHisEqualsGroupOfTicket
            (Ticket ticket, Map<String, List<ValidationError>> errors) {
        if (ticket.getExecutors() != null && !ticket.getExecutors().isEmpty()) {
            for (User executor : ticket.getExecutors()) {
                if (Boolean.FALSE.equals(executor.getGroup().getIsInner())
                && !ticket.getGroup().equals(executor.getGroup())) {
                    addValidationErrorIntoErrors(
                            EXECUTOR_IS_INVALID,
                            EXECUTOR_FROM_OUTER_GROUP_CAN_BE_IF_HIS_GROUP_EQUALS_GROUP_OF_TICKET,
                            errors
                    );
                    break;
                }
            }
        }
    }
}
