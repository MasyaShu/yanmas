package ru.itterminal.botdesk.files.service.validator;

import static ru.itterminal.botdesk.commons.util.CommonMethods.chekNumberForNullOrZero;
import static ru.itterminal.botdesk.commons.util.CommonMethods.chekNumberForNullOrMoreThan;
import static ru.itterminal.botdesk.commons.util.CommonMethods.chekObjectForNull;
import static ru.itterminal.botdesk.commons.util.CommonMethods.chekStringForNullOrEmpty;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
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
    private static final String ACCOUNT = "Account";
    private static final String ACCOUNT_NULL = "Account mustn't null";
    private static final String ENTITY_ID = "EntityId";
    private static final String ENTITY_ID_NULL = "EntityId mustn't null";

    private final Long maxSizeOfFile;

    public FileOperationValidator(@Value("${maxSizeOfFile}") Long maxSizeOfFile) {
        this.maxSizeOfFile = maxSizeOfFile;
    }

    @Override
    public boolean beforeCreate(File entity) {
        super.beforeCreate(entity);
        Map<String, List<ValidationError>> errors = new HashMap<>();
        errors.putAll(chekNumberForNullOrMoreThan(
                entity.getSize(),
                maxSizeOfFile,
                SIZE_FILE,
                SIZE_NULL,
                MAX_SIZE
        ));
        errors.putAll(chekStringForNullOrEmpty(
                entity.getFileName(),
                FILE_NAME,
                FILE_NAME_NULL,
                FILE_NAME_EMPTY
        ));
        errors.putAll(chekNumberForNullOrZero(
                entity.getCreatedAt(),
                CREATED_AT,
                CREATED_AT_NULL,
                CREATED_AT_ZERO
        ));
        errors.putAll(chekObjectForNull(
                entity.getAccount(),
                ACCOUNT,
                ACCOUNT_NULL
        ));
        errors.putAll(chekObjectForNull(
                entity.getEntityId(),
                ENTITY_ID,
                ENTITY_ID_NULL
        ));
        if (!errors.isEmpty()) {
            log.error(FIELDS_ARE_NOT_VALID, errors);
            throw new LogicalValidationException(VALIDATION_FAILED, errors);
        }
        return true;
    }
}
