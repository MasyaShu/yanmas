package ru.itterminal.botdesk.files.service.validator;

import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekNumberForNullOrMoreThan;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekNumberForNullOrZero;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekObjectForNull;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekStringForNullOrEmpty;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.ifErrorsNotEmptyThrowLogicalValidationException;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import lombok.extern.slf4j.Slf4j;
import lombok.val;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.files.model.File;

@Component
@Slf4j
public class FileOperationValidator extends BasicOperationValidatorImpl<File> {

    private static final String SIZE_FILE = "Size of file";
    private static final String MAX_SIZE = "Size of file mustn't over 25Mb";
    private static final String FILE_NAME = "Filename";
    private static final String FILE_NAME_EMPTY = "Filename mustn't empty";
    private static final String SIZE_NULL = "Size of file mustn't null";
    private static final String FILE_NAME_NULL = "Filename mustn't null";
    private static final String CREATED_AT = "Created at";
    private static final String CREATED_AT_NULL = "Created at mustn't null";
    private static final String CREATED_AT_ZERO = "Created at mustn't zero";

    private final Long maxSizeOfFile;

    public FileOperationValidator(@Value("${maxSizeOfFile}") Long maxSizeOfFile) {
        this.maxSizeOfFile = maxSizeOfFile;
    }

    @Override
    public boolean beforeCreate(File entity) {
        super.beforeCreate(entity);
        val errors = createMapForLogicalErrors();
        chekNumberForNullOrMoreThan(entity.getSize(), maxSizeOfFile, SIZE_FILE, SIZE_NULL, MAX_SIZE, errors);
        chekStringForNullOrEmpty(entity.getFileName(), FILE_NAME, FILE_NAME_NULL, FILE_NAME_EMPTY, errors);
        chekNumberForNullOrZero(entity.getCreatedAt(), CREATED_AT, CREATED_AT_NULL, CREATED_AT_ZERO, errors);
        ifErrorsNotEmptyThrowLogicalValidationException(errors);
        return true;
    }
}
