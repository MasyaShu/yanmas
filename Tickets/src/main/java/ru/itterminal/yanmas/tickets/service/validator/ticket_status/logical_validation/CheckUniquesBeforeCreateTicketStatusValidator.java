package ru.itterminal.yanmas.tickets.service.validator.ticket_status.logical_validation;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.tickets.model.TicketStatus;
import ru.itterminal.yanmas.tickets.repository.TicketStatusRepository;

import java.util.List;
import java.util.Map;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.checkStringForEquals;

@Component
@RequiredArgsConstructor
public class CheckUniquesBeforeCreateTicketStatusValidator implements EntityValidator<TicketStatus> {

    public static final String NAME = "name";
    private final TicketStatusRepository repository;

    @Override
    public void logicalValidationBeforeCreate(TicketStatus entity,
                                              Map<String, List<ValidationError>> errors) {
        var foundSetting = repository.getByNameAndAccount_Id(
                entity.getName(),
                entity.getAccount().getId()
        );
        if (!foundSetting.isEmpty()) {
            addValidationErrorIntoErrors(
                    NOT_UNIQUE_CODE,
                    format(NOT_UNIQUE_MESSAGE, THIS_NAME),
                    errors
            );
        }
    }
}
