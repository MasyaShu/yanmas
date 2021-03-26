package ru.itterminal.botdesk.tickets.service.validator;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createLogicalValidationException;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.security.jwt.JwtUserBuilder;
import ru.itterminal.botdesk.tickets.model.SettingsAccessToTicketViaTicketTypes;
import ru.itterminal.botdesk.tickets.service.impl.SettingsAccessToTicketViaTicketTypesServiceImpl;

@Slf4j
@Component
@RequiredArgsConstructor
public class SettingsAccessToTicketViaTicketTypesOperationValidator
        extends BasicOperationValidatorImpl<SettingsAccessToTicketViaTicketTypes> {

    public static final String THIS_KEY_OF_SETTINGS_ACCOUNT_ID_GROUP_ID_USER_ID =
            "This key of settings (accountId, groupId, userId)";

    private final SettingsAccessToTicketViaTicketTypesServiceImpl service;
    private final JwtUserBuilder jwtUserBuilder;

    @Override
    public void checkAccessBeforeCreate(SettingsAccessToTicketViaTicketTypes entity) {
        jwtUserBuilder.throwAccessDeniedExceptionIfCurrentUserFromOuterGroup();
    }

    @Override
    public void checkAccessBeforeUpdate(SettingsAccessToTicketViaTicketTypes entity) {
        jwtUserBuilder.throwAccessDeniedExceptionIfCurrentUserFromOuterGroup();
    }

    @SuppressWarnings("DuplicatedCode")
    @Override
    public boolean checkUniqueness(SettingsAccessToTicketViaTicketTypes entity) {
        log.trace(CHECK_UNIQUENESS, entity);
        var foundSetting = service.findByUniqueFields(entity);
        if (foundSetting.isEmpty()) {
            log.trace(FIELDS_UNIQUE, entity);
            return true;
        } else {
            log.error(format(NOT_UNIQUE_MESSAGE, THIS_KEY_OF_SETTINGS_ACCOUNT_ID_GROUP_ID_USER_ID));
            throw createLogicalValidationException(
                    NOT_UNIQUE_CODE, format(NOT_UNIQUE_MESSAGE, THIS_KEY_OF_SETTINGS_ACCOUNT_ID_GROUP_ID_USER_ID));
        }
    }
}
