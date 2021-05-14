package ru.itterminal.yanmas.files.service.validator.files.logical_validation;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.files.model.File;

import java.util.List;
import java.util.Map;

import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.chekStringForNullOrEmpty;

@Component
public class ChekNameFileForNullOrEmptyBeforeCreateValidator implements EntityValidator<File> {
    public static final String FILE_NAME = "Filename";
    public static final String FILE_NAME_EMPTY = "Filename mustn't empty";
    public static final String FILE_NAME_NULL = "Filename mustn't null";

    @Override
    public void logicalValidationBeforeCreate(File entity, Map<String, List<ValidationError>> errors) {
        chekStringForNullOrEmpty(entity.getFileName(), FILE_NAME, FILE_NAME_NULL, FILE_NAME_EMPTY, errors);
    }
}
