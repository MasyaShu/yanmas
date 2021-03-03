package ru.itterminal.botdesk.tickets.service.validator;

import static java.lang.String.format;
import static java.util.Collections.singletonList;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekObjectsIsEquals;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.ifErrorsNotEmptyThrowLogicalValidationException;

import java.util.List;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.service.impl.TicketSettingServiceImpl;

@Slf4j
@Component
public class TicketSettingOperationValidator extends BasicOperationValidatorImpl<TicketSetting> {

    public static final String GROUPS_ARENT_EQUAL = "TicketSetting.Create/Update groups aren't equal";
    public static final String GROUPS_ARENT_EQUAL_MESSAGE =
            "TicketSetting.Create/Update groups aren't equal: %s %s";
    public static final String TICKET_SETTING_UNIQUE_FIELDS = "Account, Group, Author";
    public static final String TICKET_SETTING_IS_EMPTY = "Ticket setting is empty";
    public static final String MUST_NOT_BE_EMPTY = "Ticket setting mustn't be empty";
    public static final String A_USER_FROM_NOT_INNER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_SETTING =
            "A user from not inner group cannot create or update ticket setting";

    private final TicketSettingServiceImpl service;

    public TicketSettingOperationValidator(TicketSettingServiceImpl service) {
        this.service = service;
    }

    @Override
    public boolean beforeCreate(TicketSetting entity) {
        super.beforeCreate(entity);
        checkIsInnerGroupForCreateUpdate();
        return checkBeforeCreateUpdate(entity);
    }

    @Override
    public boolean beforeUpdate(TicketSetting entity) {
        super.beforeUpdate(entity);
        checkIsInnerGroupForCreateUpdate();
        return checkBeforeCreateUpdate(entity);
    }

    @Override
    public boolean checkUniqueness(TicketSetting entity) {
        log.trace(CHECK_UNIQUENESS, entity);
        var errors = createMapForLogicalErrors();
        List<TicketSetting> foundTicketSetting = service.findByUniqueFields(entity);
        if (foundTicketSetting.isEmpty()) {
            log.trace(FIELDS_UNIQUE, entity);
            return true;
        } else {
            errors.put(TICKET_SETTING_UNIQUE_FIELDS, singletonList(
                    new ValidationError(NOT_UNIQUE_CODE, format(NOT_UNIQUE_MESSAGE, TICKET_SETTING_UNIQUE_FIELDS)))
            );
            log.error(FIELDS_NOT_UNIQUE, errors);
            throw new LogicalValidationException(VALIDATION_FAILED, errors);
        }
    }

    private boolean checkBeforeCreateUpdate(TicketSetting entity) {
        var isTicketSettingIsEmpty = true;
        var errors = createMapForLogicalErrors();

        if (entity.getAuthor() != null) {
            Group groupOfEntity = entity.getGroup();
            if (groupOfEntity != null) {
                Group groupOfAuthor = entity.getAuthor().getGroup();
                chekObjectsIsEquals(groupOfEntity, groupOfAuthor, GROUPS_ARENT_EQUAL,
                                    format(GROUPS_ARENT_EQUAL_MESSAGE, groupOfEntity, groupOfAuthor),
                                    errors
                );
            }
        }

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
            addValidationErrorIntoErrors(TICKET_SETTING_IS_EMPTY, MUST_NOT_BE_EMPTY, errors);
        }

        ifErrorsNotEmptyThrowLogicalValidationException(errors);
        return true;
    }

    private void checkIsInnerGroupForCreateUpdate() {
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (!jwtUser.isInnerGroup()) {
            throw new AccessDeniedException(A_USER_FROM_NOT_INNER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_SETTING);
        }
    }
}
