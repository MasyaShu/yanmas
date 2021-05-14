package ru.itterminal.yanmas.files.service.validator.files.logical_validation;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.files.model.File;

import java.util.List;
import java.util.Map;

import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.chekNumberForNullOrZero;

@Component
public class ChekCreatedAtForNullOrZeroBeforeCreateValidator implements EntityValidator<File> {
    public static final String CREATED_AT = "Created at";
    public static final String CREATED_AT_NULL = "Created at mustn't null";
    public static final String CREATED_AT_ZERO = "Created at mustn't zero";
    @Override
    public void logicalValidationBeforeCreate(File entity, Map<String, List<ValidationError>> errors) {
        chekNumberForNullOrZero(entity.getCreatedAt(), CREATED_AT, CREATED_AT_NULL, CREATED_AT_ZERO, errors);
    }
}
