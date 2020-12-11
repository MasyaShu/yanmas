package ru.itterminal.botdesk.tickets.service.validator;

import static java.lang.String.format;
import static java.util.Collections.singletonList;
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

    private static final String ACCOUNTS_ARENT_EQUAL = "TicketSetting.Create/Update accounts aren't equal";
    private static final String GROUPS_ARENT_EQUAL = "TicketSetting.Create/Update groups aren't equal";
    private static final String ACCOUNTS_ARENT_EQUAL_MESSAGE =
            "TicketSetting.Create/Update accounts aren't equal: %s %s";
    private static final String GROUPS_ARENT_EQUAL_MESSAGE =
            "TicketSetting.Create/Update groups aren't equal: %s %s";

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
            String validatedFields = "account, group, author";
            errors.put(validatedFields, singletonList(
                    new ValidationError(NOT_UNIQUE_CODE, format(NOT_UNIQUE_MESSAGE, validatedFields)))
            );
            log.error(FIELDS_NOT_UNIQUE, errors);
            throw new LogicalValidationException(VALIDATION_FAILED, errors);
        }
    }

    private boolean checkBeforeCreateUpdate(TicketSetting entity) {
        var errors = createMapForLogicalErrors();
        Account account = entity.getAccount();

        Account accountOfGroup = entity.getGroup().getAccount();
        errors.putAll(chekObjectsIsEquals(account, accountOfGroup, ACCOUNTS_ARENT_EQUAL,
                                          format(ACCOUNTS_ARENT_EQUAL_MESSAGE, account, entity.getGroup())
                      )
        );

        Account accountOfAuthor = entity.getAuthor().getAccount();
        errors.putAll(chekObjectsIsEquals(account, accountOfAuthor, ACCOUNTS_ARENT_EQUAL,
                                          format(ACCOUNTS_ARENT_EQUAL_MESSAGE, account, entity.getAuthor())
                      )
        );

        Group groupOfEntity = entity.getGroup();
        Group groupOfAuthor = entity.getAuthor().getOwnGroup();
        errors.putAll(chekObjectsIsEquals(groupOfEntity, groupOfAuthor, GROUPS_ARENT_EQUAL,
                                          format(GROUPS_ARENT_EQUAL_MESSAGE, groupOfEntity, groupOfAuthor)
                      )
        );

        Account accountOfTicketTypeForNew = entity.getTicketTypeForNew().getAccount();
        errors.putAll(chekObjectsIsEquals(account, accountOfTicketTypeForNew, ACCOUNTS_ARENT_EQUAL,
                                          format(ACCOUNTS_ARENT_EQUAL_MESSAGE, account, entity.getTicketTypeForNew())
                      )
        );

        Account accountOfTicketStatusForNew = entity.getTicketStatusForNew().getAccount();
        errors.putAll(chekObjectsIsEquals(account, accountOfTicketStatusForNew, ACCOUNTS_ARENT_EQUAL,
                                          format(ACCOUNTS_ARENT_EQUAL_MESSAGE, account, entity.getTicketStatusForNew())
                      )
        );

        Account accountOfTicketStatusForReopen = entity.getTicketStatusForReopen().getAccount();
        errors.putAll(chekObjectsIsEquals(account, accountOfTicketStatusForReopen, ACCOUNTS_ARENT_EQUAL,
                                          format(ACCOUNTS_ARENT_EQUAL_MESSAGE, account, entity.getTicketStatusForReopen())
                      )
        );

        Account accountOfTicketStatusForClose = entity.getTicketStatusForClose().getAccount();
        errors.putAll(chekObjectsIsEquals(account, accountOfTicketStatusForClose, ACCOUNTS_ARENT_EQUAL,
                                          format(ACCOUNTS_ARENT_EQUAL_MESSAGE, account, entity.getTicketStatusForClose())
                      )
        );

        Account accountOfTicketStatusForCancel = entity.getTicketStatusForCancel().getAccount();
        errors.putAll(chekObjectsIsEquals(account, accountOfTicketStatusForCancel, ACCOUNTS_ARENT_EQUAL,
                                          format(ACCOUNTS_ARENT_EQUAL_MESSAGE, account, entity.getTicketStatusForCancel())
                      )
        );

        for (User observer: entity.getObservers()) {
            Account accountOfObserver = observer.getAccount();
            errors.putAll(chekObjectsIsEquals(account, accountOfObserver, ACCOUNTS_ARENT_EQUAL,
                                              format(ACCOUNTS_ARENT_EQUAL_MESSAGE, account, observer)
                          )
            );
        }

        for (User executor: entity.getExecutors()) {
            Account accountOfExecutor = executor.getAccount();
            errors.putAll(chekObjectsIsEquals(account, accountOfExecutor, ACCOUNTS_ARENT_EQUAL,
                                              format(ACCOUNTS_ARENT_EQUAL_MESSAGE, account, executor)
                          )
            );
        }

        ifErrorsNotEmptyThrowLogicalValidationException(errors);
        return true;
    }
}
