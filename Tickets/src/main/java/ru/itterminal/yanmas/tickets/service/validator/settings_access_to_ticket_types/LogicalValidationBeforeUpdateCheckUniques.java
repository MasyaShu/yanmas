package ru.itterminal.yanmas.tickets.service.validator.settings_access_to_ticket_types;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

import java.util.List;
import java.util.Map;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.tickets.model.SettingsAccessToTicketTypes;
import ru.itterminal.yanmas.tickets.repository.SettingsAccessToTicketTypesRepository;

@Component
@RequiredArgsConstructor
public class LogicalValidationBeforeUpdateCheckUniques implements EntityValidator<SettingsAccessToTicketTypes> {

    private final SettingsAccessToTicketTypesRepository repository;

    @Override
    public void logicalValidationBeforeUpdate(SettingsAccessToTicketTypes entity,
                                              Map<String, List<ValidationError>> errors) {
        var foundSetting = repository.findAllByAccount_IdAndGroup_IdAndUser_IdAndIdNot(
                entity.getAccount().getId(),
                entity.getGroup() == null ? null : entity.getGroup().getId(),
                entity.getUser() == null ? null : entity.getUser().getId(),
                entity.getId()
        );
        if (!foundSetting.isEmpty()) {
            addValidationErrorIntoErrors(
                    NOT_UNIQUE_CODE,
                    format(NOT_UNIQUE_MESSAGE, THIS_KEY_OF_SETTINGS_ACCOUNT_ID_GROUP_ID_USER_ID),
                    errors
            );
        }
    }

}
