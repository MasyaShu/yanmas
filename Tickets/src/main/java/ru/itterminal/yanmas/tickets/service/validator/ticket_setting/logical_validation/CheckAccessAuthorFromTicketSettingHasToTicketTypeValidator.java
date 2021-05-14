package ru.itterminal.yanmas.tickets.service.validator.ticket_setting.logical_validation;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.tickets.model.TicketSetting;
import ru.itterminal.yanmas.tickets.service.impl.SettingsAccessToTicketTypesServiceImpl;

import java.util.List;
import java.util.Map;

import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

@Component
@RequiredArgsConstructor
public class CheckAccessAuthorFromTicketSettingHasToTicketTypeValidator implements EntityValidator<TicketSetting> {

    public static final String INVALID_TICKET_SETTINGS = "Invalid ticket settings";
    public static final String INVALID_TICKET_SETTINGS_BECAUSE_AUTHOR_HAS_NOT_ACCESS_TO_TICKET_TYPE =
            "Invalid ticket settings, because author has not access to ticket type";

    private final SettingsAccessToTicketTypesServiceImpl settingsAccessToTicketTypesService;

    @Override
    public void logicalValidationBeforeCreate(TicketSetting entity, Map<String, List<ValidationError>> errors) {
        checkAccessAuthorFromTicketSettingHasToTicketType(entity, errors);
    }

    @Override
    public void logicalValidationBeforeUpdate(TicketSetting entity, Map<String, List<ValidationError>> errors) {
        checkAccessAuthorFromTicketSettingHasToTicketType(entity, errors);
    }

    private void checkAccessAuthorFromTicketSettingHasToTicketType(TicketSetting entity, Map<String, List<ValidationError>> errors) {
        if (entity.getTicketTypeForNew() != null && entity.getAuthor() != null) {
            var authorId = entity.getAuthor().getId();
            var ticketTypeId = entity.getTicketTypeForNew().getId();
            if (!settingsAccessToTicketTypesService.isPermittedTicketType(ticketTypeId, authorId)) {
                addValidationErrorIntoErrors(
                        INVALID_TICKET_SETTINGS, INVALID_TICKET_SETTINGS_BECAUSE_AUTHOR_HAS_NOT_ACCESS_TO_TICKET_TYPE,
                        errors
                );
            }
        }
    }

}
