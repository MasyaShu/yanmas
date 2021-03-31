package ru.itterminal.botdesk.tickets.service.validator;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createLogicalValidationException;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.ifErrorsNotEmptyThrowLogicalValidationException;

import java.util.List;
import java.util.UUID;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.security.jwt.JwtUserBuilder;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.service.impl.SettingsAccessToTicketTypesServiceImpl;
import ru.itterminal.botdesk.tickets.service.impl.TicketSettingServiceImpl;

@Slf4j
@Component
@RequiredArgsConstructor
public class TicketSettingOperationValidator extends BasicOperationValidatorImpl<TicketSetting> {

    public static final String TICKET_SETTING_UNIQUE_FIELDS = "The key of settings (accountId, groupId, authorId)";
    public static final String TICKET_SETTING_IS_EMPTY = "Ticket setting is empty";
    public static final String TICKET_SETTING_MUST_NOT_BE_EMPTY = "Ticket setting mustn't be empty";
    public static final String A_USER_CANNOT_GET_SETTING_OR_PREDEFINED_VALUES_FOR_TICKET =
            "A user cannot get setting or predefined values for ticket if his group is not equal group of author from request";
    public static final String ACCESS_TO_TICKET_TYPE = "Access to ticket type";
    public static final String AUTHOR_HAS_NOT_ACCESS_TO_TICKET_TYPE = "Author has not access to ticket type";
    public static final String CURRENT_USER_HAS_NOT_ACCESS_TO_TICKET_TYPE = "Current user has not access to ticket "
            + "type";

    private final TicketSettingServiceImpl service;
    private final SettingsAccessToTicketTypesServiceImpl settingsAccessToTicketTypesService;
    private final JwtUserBuilder jwtUserBuilder;

    @Override
    public void checkAccessBeforeCreate(TicketSetting entity) {
        checkAccessBeforeCreateUpdate();
    }

    @Override
    public boolean logicalValidationBeforeCreate(TicketSetting entity) {
        super.logicalValidationBeforeCreate(entity);
        return logicalValidationBeforeCreateUpdate(entity);
    }

    @Override
    public void checkAccessBeforeUpdate(TicketSetting entity) {
        checkAccessBeforeCreateUpdate();
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

    private boolean logicalValidationBeforeCreateUpdate(TicketSetting entity) {
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
            addValidationErrorIntoErrors(TICKET_SETTING_IS_EMPTY, TICKET_SETTING_MUST_NOT_BE_EMPTY, errors);
        }

        if (entity.getTicketTypeForNew() != null && entity.getAuthor() != null) {
            var ticketTypeId = entity.getTicketTypeForNew().getId();
            var userId = entity.getAuthor().getId();
            if (!settingsAccessToTicketTypesService.isPermittedTicketType(ticketTypeId, userId)) {
                addValidationErrorIntoErrors(ACCESS_TO_TICKET_TYPE, AUTHOR_HAS_NOT_ACCESS_TO_TICKET_TYPE, errors);
            }
        }

        if (entity.getTicketTypeForNew() != null) {
            var ticketTypeId = entity.getTicketTypeForNew().getId();
            var currentUserId = jwtUserBuilder.getJwtUser().getId();
            if (!settingsAccessToTicketTypesService.isPermittedTicketType(ticketTypeId, currentUserId)) {
                addValidationErrorIntoErrors(ACCESS_TO_TICKET_TYPE, CURRENT_USER_HAS_NOT_ACCESS_TO_TICKET_TYPE, errors);
            }
            if (entity.getAuthor() != null) {
                var authorId = entity.getAuthor().getId();
                if (!settingsAccessToTicketTypesService.isPermittedTicketType(ticketTypeId, authorId)) {
                    addValidationErrorIntoErrors(ACCESS_TO_TICKET_TYPE, AUTHOR_HAS_NOT_ACCESS_TO_TICKET_TYPE, errors);
                }
            }
        }

        ifErrorsNotEmptyThrowLogicalValidationException(errors);
        return true;
    }

    private void checkAccessBeforeCreateUpdate() {
        jwtUserBuilder.throwAccessDeniedExceptionIfCurrentUserFromOuterGroup();
    }

    public void checkAccessForGetSettingOrPredefinedValuesForTicket(UUID groupIdOfAuthor) {
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (!jwtUser.getGroupId().equals(groupIdOfAuthor)) {
            throw new AccessDeniedException(A_USER_CANNOT_GET_SETTING_OR_PREDEFINED_VALUES_FOR_TICKET);
        }
    }
}
