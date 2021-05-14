package ru.itterminal.yanmas.tickets.service.validator.ticket_setting.logical_validation;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.tickets.model.TicketSetting;

import java.util.List;
import java.util.Map;

import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

@Component
public class CheckTicketSettingMustNotBeEmptyValidator implements EntityValidator<TicketSetting> {

    public static final String INVALID_TICKET_SETTINGS = "Invalid ticket settings";
    public static final String TICKET_SETTING_MUST_NOT_BE_EMPTY = "Ticket setting mustn't be empty";


    @Override
    public void logicalValidationBeforeCreate(TicketSetting entity, Map<String, List<ValidationError>> errors) {
        checkTicketSettingMustNotBeEmpty(entity, errors);
    }

    @Override
    public void logicalValidationBeforeUpdate(TicketSetting entity, Map<String, List<ValidationError>> errors) {
        checkTicketSettingMustNotBeEmpty(entity, errors);
    }

    private void checkTicketSettingMustNotBeEmpty(TicketSetting entity, Map<String, List<ValidationError>> errors) {
        var isTicketSettingIsEmpty = entity.getTicketTypeForNew() == null;

        if (entity.getTicketStatusForNew() != null) {
            isTicketSettingIsEmpty = false;
        }

        if (entity.getTicketStatusForReopen() != null) {
            isTicketSettingIsEmpty = false;
        }

        if (entity.getTicketStatusForClose() != null) {
            isTicketSettingIsEmpty = false;
        }

        if (entity.getTicketStatusForCancel() != null) {
            isTicketSettingIsEmpty = false;
        }

        if (entity.getObservers() != null
                && !entity.getObservers().isEmpty()) {
            isTicketSettingIsEmpty = false;
        }

        if (entity.getExecutors() != null
                && !entity.getExecutors().isEmpty()) {
            isTicketSettingIsEmpty = false;
        }

        if (isTicketSettingIsEmpty) {
            addValidationErrorIntoErrors(INVALID_TICKET_SETTINGS,
                    TICKET_SETTING_MUST_NOT_BE_EMPTY,
                    errors);
        }
    }
}
