package ru.itterminal.yanmas.tickets.service.validator;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createLogicalValidationException;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.BasicOperationValidatorWithCurrentUserImpl;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;
import ru.itterminal.yanmas.tickets.model.SettingsAccessToTicketTypes;
import ru.itterminal.yanmas.tickets.repository.SettingsAccessToTicketTypesRepository;

@Slf4j
@Component
@RequiredArgsConstructor
public class SettingsAccessToTicketTypesOperationValidator
        extends BasicOperationValidatorWithCurrentUserImpl<SettingsAccessToTicketTypes> {

    public static final String THIS_KEY_OF_SETTINGS_ACCOUNT_ID_GROUP_ID_USER_ID =
            "This key of settings (accountId, groupId, userId)";
    public static final String START_FIND = "Start {} , {}, {}";

    private final JwtUserBuilder jwtUserBuilder;
    private final SettingsAccessToTicketTypesRepository repository;

    @Override
    public void checkAccessBeforeCreate(SettingsAccessToTicketTypes entity, User currentUser) {
        jwtUserBuilder.throwAccessDeniedExceptionIfCurrentUserFromOuterGroup();
    }

    @Override
    public void checkAccessBeforeUpdate(SettingsAccessToTicketTypes entity, User currentUser) {
        jwtUserBuilder.throwAccessDeniedExceptionIfCurrentUserFromOuterGroup();
    }

    @Override
    public void checkAccessBeforeRead(SettingsAccessToTicketTypes entity, User currentUser) {
        jwtUserBuilder.throwAccessDeniedExceptionIfCurrentUserFromOuterGroup();
    }



    @SuppressWarnings("DuplicatedCode")
    @Override
    public boolean checkUniqueness(SettingsAccessToTicketTypes entity) {
        log.trace(CHECK_UNIQUENESS, entity);
        log.trace(START_FIND, entity.getAccount(), entity.getGroup(), entity.getUser());
        var foundSetting = repository.findAllByAccount_IdAndGroup_IdAndUser_IdAndIdNot(
                entity.getAccount().getId(),
                entity.getGroup() == null ? null : entity.getGroup().getId(),
                entity.getUser() == null ? null : entity.getUser().getId(),
                entity.getId()
        );
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
