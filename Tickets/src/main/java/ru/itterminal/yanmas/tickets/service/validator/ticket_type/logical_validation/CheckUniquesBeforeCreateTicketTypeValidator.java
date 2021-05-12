package ru.itterminal.yanmas.tickets.service.validator.ticket_type.logical_validation;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.tickets.model.TicketType;
import ru.itterminal.yanmas.tickets.repository.TicketTypeRepository;

import java.util.List;
import java.util.Map;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

@Component
@RequiredArgsConstructor
public class CheckUniquesBeforeCreateTicketTypeValidator implements EntityValidator<TicketType> {

    private final TicketTypeRepository repository;

    @Override
    public void logicalValidationBeforeCreate(TicketType entity, Map<String, List<ValidationError>> errors) {

        var foundTicketTypes = repository.getByNameAndAccount_Id(
                entity.getName(),
                entity.getAccount().getId()
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
