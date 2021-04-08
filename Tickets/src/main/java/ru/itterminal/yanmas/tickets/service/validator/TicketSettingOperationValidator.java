package ru.itterminal.yanmas.tickets.service.validator;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createLogicalValidationException;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.ifErrorsNotEmptyThrowLogicalValidationException;

import java.util.List;
import java.util.UUID;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.yanmas.security.jwt.JwtUser;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;
import ru.itterminal.yanmas.tickets.model.TicketSetting;
import ru.itterminal.yanmas.tickets.service.impl.SettingsAccessToTicketTypesServiceImpl;
import ru.itterminal.yanmas.tickets.service.impl.TicketSettingServiceImpl;

@Slf4j
@Component
@RequiredArgsConstructor
public class TicketSettingOperationValidator extends BasicOperationValidatorImpl<TicketSetting> {

    public static final String TICKET_SETTING_UNIQUE_FIELDS = "The key of settings (accountId, groupId, authorId)";
    public static final String TICKET_SETTING_MUST_NOT_BE_EMPTY = "Ticket setting mustn't be empty";
    public static final String A_USER_CANNOT_GET_SETTING_OR_PREDEFINED_VALUES_FOR_TICKET =
            "A user cannot get setting or predefined values for ticket if his group is not equal group of author from request";
    public static final String INVALID_TICKET_SETTINGS = "Invalid ticket settings";
    public static final String INVALID_TICKET_SETTINGS_BECAUSE_AUTHOR_HAS_NOT_ACCESS_TO_TICKET_TYPE =
            "Invalid ticket settings, because author has not access to ticket type";
    public static final String ACCESS_DENIED_BECAUSE_CURRENT_USER_HAS_NOT_PERMIT_TO_TICKET_TYPE =
            "Access denied, because current user has not permit to ticket type";

    private final TicketSettingServiceImpl service;
    private final SettingsAccessToTicketTypesServiceImpl settingsAccessToTicketTypesService;
    private final JwtUserBuilder jwtUserBuilder;

    @Override
    public void checkAccessBeforeCreate(TicketSetting entity) {
        checkAccessBeforeCreateUpdate(entity);
    }

    @Override
    public boolean logicalValidationBeforeCreate(TicketSetting entity) {
        super.logicalValidationBeforeCreate(entity);
        return logicalValidationBeforeCreateUpdate(entity);
    }

    @Override
    public void checkAccessBeforeUpdate(TicketSetting entity) {
        checkAccessBeforeCreateUpdate(entity);
    }

    @Override
    public boolean logicalValidationBeforeUpdate(TicketSetting entity) {
        super.logicalValidationBeforeUpdate(entity);
        return logicalValidationBeforeCreateUpdate(entity);
    }

    @Override
    public boolean checkUniqueness(TicketSetting entity) {
        log.trace(CHECK_UNIQUENESS, entity);
        List<TicketSetting> foundTicketSetting = service.findByUniqueFields(entity);
        if (foundTicketSetting.isEmpty()) {
            log.trace(FIELDS_UNIQUE, entity);
            return true;
        } else {
            log.error(format(NOT_UNIQUE_MESSAGE, format(NOT_UNIQUE_MESSAGE, TICKET_SETTING_UNIQUE_FIELDS)));
            throw createLogicalValidationException(
                    NOT_UNIQUE_CODE, format(NOT_UNIQUE_MESSAGE, TICKET_SETTING_UNIQUE_FIELDS));
        }
    }

    private boolean logicalValidationBeforeCreateUpdate(TicketSetting entity) { //NOSONAR
        var isTicketSettingIsEmpty = true;
        var errors = createMapForLogicalErrors();

        if (entity.getTicketTypeForNew() != null) {
            isTicketSettingIsEmpty = false;
        }

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

        if (entity.getObservers() != null && !entity.getObservers().isEmpty()) {
            isTicketSettingIsEmpty = false;
        }

        if (entity.getExecutors() != null && !entity.getExecutors().isEmpty()) {
            isTicketSettingIsEmpty = false;
        }

        if (isTicketSettingIsEmpty) {
            addValidationErrorIntoErrors(INVALID_TICKET_SETTINGS, TICKET_SETTING_MUST_NOT_BE_EMPTY, errors);
        }

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

        ifErrorsNotEmptyThrowLogicalValidationException(errors);
        return true;
    }

    private void checkAccessBeforeCreateUpdate(TicketSetting entity) {
        jwtUserBuilder.throwAccessDeniedExceptionIfCurrentUserFromOuterGroup();
        if (entity.getTicketTypeForNew() != null) {
            var ticketTypeId = entity.getTicketTypeForNew().getId();
            var currentUserId = jwtUserBuilder.getJwtUser().getId();
            if (!settingsAccessToTicketTypesService.isPermittedTicketType(ticketTypeId, currentUserId)) {
                throw new AccessDeniedException(ACCESS_DENIED_BECAUSE_CURRENT_USER_HAS_NOT_PERMIT_TO_TICKET_TYPE);
            }
        }
    }

    public void checkAccessForGetSettingOrPredefinedValuesForTicket(UUID groupIdOfAuthor) {
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (!jwtUser.getGroupId().equals(groupIdOfAuthor)) {
            throw new AccessDeniedException(A_USER_CANNOT_GET_SETTING_OR_PREDEFINED_VALUES_FOR_TICKET);
        }
    }
}
