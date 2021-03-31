package ru.itterminal.botdesk.files.service.validator;

import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekNumberForNullOrZero;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekStringForNullOrEmpty;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.ifErrorsNotEmptyThrowLogicalValidationException;

import org.springframework.stereotype.Component;

import lombok.extern.slf4j.Slf4j;
import lombok.val;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.files.model.File;

@Component
@Slf4j
public class FileOperationValidator extends BasicOperationValidatorImpl<File> {

    public static final String FILE_NAME = "Filename";
    public static final String FILE_NAME_EMPTY = "Filename mustn't empty";
    public static final String FILE_NAME_NULL = "Filename mustn't null";
    public static final String CREATED_AT = "Created at";
    public static final String CREATED_AT_NULL = "Created at mustn't null";
    public static final String CREATED_AT_ZERO = "Created at mustn't zero";

    @Override
    public boolean logicalValidationBeforeCreate(File entity) {
        super.logicalValidationBeforeCreate(entity);
        val errors = createMapForLogicalErrors();
        chekStringForNullOrEmpty(entity.getFileName(), FILE_NAME, FILE_NAME_NULL, FILE_NAME_EMPTY, errors);
        chekNumberForNullOrZero(entity.getCreatedAt(), CREATED_AT, CREATED_AT_NULL, CREATED_AT_ZERO, errors);
        ifErrorsNotEmptyThrowLogicalValidationException(errors);
        return true;
    }
}
