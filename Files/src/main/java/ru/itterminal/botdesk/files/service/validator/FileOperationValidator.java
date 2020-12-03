package ru.itterminal.botdesk.files.service.validator;

import static java.util.Collections.singletonList;

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

    private final Long maxSizeOfFile;

    public FileOperationValidator(@Value("${maxSizeOfFile}") Long maxSizeOfFile) {
        this.maxSizeOfFile = maxSizeOfFile;
    }

    @Override
    public boolean beforeCreate(File entity) {
        super.beforeCreate(entity);
        Map<String, List<ValidationError>> errors = new HashMap<>();
        if (entity.getSize() > maxSizeOfFile) {
            errors.put(SIZE_FILE, singletonList(new ValidationError(SIZE_FILE, MAX_SIZE)));
        }
        if (!errors.isEmpty()) {
            log.error(FIELDS_ARE_NOT_VALID, errors);
            throw new LogicalValidationException(VALIDATION_FAILED, errors);
        }
        return true;
    }
}
