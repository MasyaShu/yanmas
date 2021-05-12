package ru.itterminal.yanmas.tickets.service.validator.ticket_setting.logical_validation;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.tickets.model.TicketSetting;
import ru.itterminal.yanmas.tickets.repository.TicketSettingRepository;

import java.util.List;
import java.util.Map;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

@Component
@RequiredArgsConstructor
public class CheckUniquesBeforeUpdateTicketSettingValidator implements EntityValidator<TicketSetting> {

    public static final String TICKET_SETTING_UNIQUE_FIELDS = "The key of settings (accountId, groupId, authorId)";

    private final TicketSettingRepository repository;

    @Override
    public void logicalValidationBeforeUpdate(TicketSetting entity, Map<String, List<ValidationError>> errors) {

        var foundTicketSetting = repository.findAllByAccount_IdAndGroup_IdAndAuthor_IdAndIdNot(
                entity.getAccount().getId(),
                entity.getGroup() == null ? null : entity.getGroup().getId(),
                entity.getAuthor() == null ? null : entity.getAuthor().getId(),
                entity.getId()
        );
        if (!foundTicketSetting.isEmpty()) {
            addValidationErrorIntoErrors(
                    NOT_UNIQUE_CODE,
                    format(NOT_UNIQUE_MESSAGE, TICKET_SETTING_UNIQUE_FIELDS),
                    errors
            );
        }
    }
}
