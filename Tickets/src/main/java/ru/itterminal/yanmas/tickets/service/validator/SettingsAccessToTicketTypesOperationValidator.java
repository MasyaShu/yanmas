package ru.itterminal.yanmas.tickets.service.validator;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createLogicalValidationException;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;
import ru.itterminal.yanmas.tickets.model.SettingsAccessToTicketTypes;
import ru.itterminal.yanmas.tickets.service.impl.SettingsAccessToTicketTypesServiceImpl;

@Slf4j
@Component
@RequiredArgsConstructor
public class SettingsAccessToTicketTypesOperationValidator
        extends BasicOperationValidatorImpl<SettingsAccessToTicketTypes> {

    public static final String THIS_KEY_OF_SETTINGS_ACCOUNT_ID_GROUP_ID_USER_ID =
            "This key of settings (accountId, groupId, userId)";

    private final SettingsAccessToTicketTypesServiceImpl service;
    private final JwtUserBuilder jwtUserBuilder;

    @Override
    public void checkAccessBeforeCreate(SettingsAccessToTicketTypes entity) {
        jwtUserBuilder.throwAccessDeniedExceptionIfCurrentUserFromOuterGroup();
    }

    @Override
    public void checkAccessBeforeUpdate(SettingsAccessToTicketTypes entity) {
        jwtUserBuilder.throwAccessDeniedExceptionIfCurrentUserFromOuterGroup();
    }

    @Override
    public void checkAccessBeforeRead(SettingsAccessToTicketTypes entity) {
        jwtUserBuilder.throwAccessDeniedExceptionIfCurrentUserFromOuterGroup();
    }

    @SuppressWarnings("DuplicatedCode")
    @Override
    public boolean checkUniqueness(SettingsAccessToTicketTypes entity) {
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
