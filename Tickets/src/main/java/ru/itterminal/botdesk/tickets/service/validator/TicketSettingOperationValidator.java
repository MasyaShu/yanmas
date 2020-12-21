package ru.itterminal.botdesk.tickets.service.validator;

import static java.lang.String.format;
import static java.util.Collections.singletonList;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekObjectsIsEquals;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.ifErrorsNotEmptyThrowLogicalValidationException;

import java.util.List;

import org.springframework.stereotype.Component;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.projection.TicketSettingUniqueFields;
import ru.itterminal.botdesk.tickets.service.impl.TicketSettingServiceImpl;

@Slf4j
@Component
public class TicketSettingOperationValidator extends BasicOperationValidatorImpl<TicketSetting> {

    public static final String ACCOUNTS_ARENT_EQUAL = "TicketSetting.Create/Update accounts aren't equal";
    public static final String GROUPS_ARENT_EQUAL = "TicketSetting.Create/Update groups aren't equal";
    public static final String ACCOUNTS_ARENT_EQUAL_MESSAGE =
            "TicketSetting.Create/Update accounts aren't equal: %s %s";
    public static final String GROUPS_ARENT_EQUAL_MESSAGE =
            "TicketSetting.Create/Update groups aren't equal: %s %s";
    public static final String TICKET_SETTING_UNIQUE_FIELDS = "Account, Group, Author";
    public static final String TICKET_SETTING_IS_EMPTY = "Ticket setting is empty";
    public static final String MUST_NOT_BE_EMPTY = "Ticket setting mustn't be empty";

    private final TicketSettingServiceImpl service;

    public TicketSettingOperationValidator(TicketSettingServiceImpl service) {
        this.service = service;
    }

    @Override
    public boolean beforeCreate(TicketSetting entity) {
        super.beforeCreate(entity);
        return checkBeforeCreateUpdate(entity);
    }

    @Override
    public boolean beforeUpdate(TicketSetting entity) {
        super.beforeUpdate(entity);
        return checkBeforeCreateUpdate(entity);
    }

    @Override
    public boolean checkUniqueness(TicketSetting entity) {
        log.trace(CHECK_UNIQUENESS, entity);
        var errors = createMapForLogicalErrors();
        List<TicketSettingUniqueFields> foundTicketSetting = service.findByUniqueFields(entity);
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

        Account account = entity.getAccount();

        if (entity.getGroup() != null) {
            Account accountOfGroup = entity.getGroup().getAccount();
            chekObjectsIsEquals(account, accountOfGroup, ACCOUNTS_ARENT_EQUAL,
                                format(ACCOUNTS_ARENT_EQUAL_MESSAGE, account, entity.getGroup()),
                                errors
            );
        }

        if (entity.getAuthor() != null) {
            Account accountOfAuthor = entity.getAuthor().getAccount();
            chekObjectsIsEquals(account, accountOfAuthor, ACCOUNTS_ARENT_EQUAL,
                                format(ACCOUNTS_ARENT_EQUAL_MESSAGE, account, entity.getAuthor()),
                                errors
            );

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
            Account accountOfTicketTypeForNew = entity.getTicketTypeForNew().getAccount();
            chekObjectsIsEquals(account, accountOfTicketTypeForNew, ACCOUNTS_ARENT_EQUAL,
                                format(
                                        ACCOUNTS_ARENT_EQUAL_MESSAGE, account,
                                        entity.getTicketTypeForNew()
                                ),
                                errors
            );
        }

        if (entity.getTicketStatusForNew() != null) {
            isTicketSettingIsEmpty = false;
            Account accountOfTicketStatusForNew = entity.getTicketStatusForNew().getAccount();
            chekObjectsIsEquals(account, accountOfTicketStatusForNew, ACCOUNTS_ARENT_EQUAL,
                                format(
                                        ACCOUNTS_ARENT_EQUAL_MESSAGE, account,
                                        entity.getTicketStatusForNew()
                                ),
                                errors
            );
        }

        if (entity.getTicketStatusForReopen() != null) {
            isTicketSettingIsEmpty = false;
            Account accountOfTicketStatusForReopen = entity.getTicketStatusForReopen().getAccount();
            chekObjectsIsEquals(account, accountOfTicketStatusForReopen, ACCOUNTS_ARENT_EQUAL,
                                format(
                                        ACCOUNTS_ARENT_EQUAL_MESSAGE, account,
                                        entity.getTicketStatusForReopen()
                                ),
                                errors
            );
        }

        if (entity.getTicketStatusForClose() != null) {
            isTicketSettingIsEmpty = false;
            Account accountOfTicketStatusForClose = entity.getTicketStatusForClose().getAccount();
            chekObjectsIsEquals(account, accountOfTicketStatusForClose, ACCOUNTS_ARENT_EQUAL,
                                format(
                                        ACCOUNTS_ARENT_EQUAL_MESSAGE, account,
                                        entity.getTicketStatusForClose()
                                ),
                                errors
            );
        }

        if (entity.getTicketStatusForCancel() != null) {
            isTicketSettingIsEmpty = false;
            Account accountOfTicketStatusForCancel = entity.getTicketStatusForCancel().getAccount();
            chekObjectsIsEquals(account, accountOfTicketStatusForCancel, ACCOUNTS_ARENT_EQUAL,
                                format(
                                        ACCOUNTS_ARENT_EQUAL_MESSAGE, account,
                                        entity.getTicketStatusForCancel()
                                ),
                                errors
            );
        }

        if (entity.getObservers() != null && !entity.getObservers().isEmpty()) {
            isTicketSettingIsEmpty = false;
        }

        if (entity.getObservers() != null) {
            for (User observer : entity.getObservers()) {
                Account accountOfObserver = observer.getAccount();
                chekObjectsIsEquals(account, accountOfObserver, ACCOUNTS_ARENT_EQUAL,
                                    format(ACCOUNTS_ARENT_EQUAL_MESSAGE, account, observer),
                                    errors
                );
            }
        }

        if (entity.getExecutors() != null && !entity.getExecutors().isEmpty()) {
            isTicketSettingIsEmpty = false;
        }

        if (entity.getExecutors() != null) {
            for (User executor : entity.getExecutors()) {
                Account accountOfExecutor = executor.getAccount();
                chekObjectsIsEquals(account, accountOfExecutor, ACCOUNTS_ARENT_EQUAL,
                                    format(ACCOUNTS_ARENT_EQUAL_MESSAGE, account, executor),
                                    errors
                );
            }
        }

        if (isTicketSettingIsEmpty) {
            addValidationErrorIntoErrors(TICKET_SETTING_IS_EMPTY, MUST_NOT_BE_EMPTY, errors);
        }

        ifErrorsNotEmptyThrowLogicalValidationException(errors);
        return true;
    }
}
