package ru.itterminal.yanmas.tickets.service.validator.group_ticket_types.logical_validation;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.tickets.model.GroupTicketTypes;
import ru.itterminal.yanmas.tickets.repository.GroupTicketTypesRepository;

import java.util.List;
import java.util.Map;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

@Component
@RequiredArgsConstructor
public class CheckUniquesBeforeUpdateGroupTicketTypeValidator implements EntityValidator<GroupTicketTypes> {

    private final GroupTicketTypesRepository repository;

    @Override
    public void logicalValidationBeforeUpdate(GroupTicketTypes entity, Map<String, List<ValidationError>> errors) {

        var foundTicketTypes = repository.getByNameAndAccount_IdAndIdNot(
                entity.getName(),
                entity.getAccount().getId(),
                entity.getId()
        );
        if (!foundTicketTypes.isEmpty()) {
            addValidationErrorIntoErrors(
                    NOT_UNIQUE_CODE,
                    format(NOT_UNIQUE_MESSAGE, THIS_NAME),
                    errors
            );
        }
    }
}
