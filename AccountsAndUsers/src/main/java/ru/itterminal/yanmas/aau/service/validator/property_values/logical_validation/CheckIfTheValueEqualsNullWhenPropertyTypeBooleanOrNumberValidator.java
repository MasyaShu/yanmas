package ru.itterminal.yanmas.aau.service.validator.property_values.logical_validation;

import ru.itterminal.yanmas.aau.model.PropertyValues;
import ru.itterminal.yanmas.aau.model.TypeProperty;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;

import java.util.List;
import java.util.Map;

import static ru.itterminal.yanmas.commons.util.CommonConstants.MUST_NOT_BE_NULL;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

public class CheckIfTheValueEqualsNullWhenPropertyTypeBooleanOrNumberValidator implements EntityValidator<PropertyValues> {

    public static final String ERROR_VALUE = "error value";

    @Override
    public void logicalValidationBeforeUpdate(PropertyValues entity, Map<String, List<ValidationError>> errors) {
        checkIfTheValueMatchesThePropertyType(entity, errors);
    }

    @Override
    public void logicalValidationBeforeCreate(PropertyValues entity, Map<String, List<ValidationError>> errors) {
        checkIfTheValueMatchesThePropertyType(entity, errors);
    }

    private void checkIfTheValueMatchesThePropertyType(PropertyValues entity, Map<String, List<ValidationError>> errors) {
        if ((entity.getProperty().getTypeProperty().equals(TypeProperty.NUMBER.toString())
                || entity.getProperty().getTypeProperty().equals(TypeProperty.BOOLEAN.toString()))
                && entity.getValue() == null) {
            addValidationErrorIntoErrors(
                    ERROR_VALUE,
                    MUST_NOT_BE_NULL,
                    errors
            );
        }
    }
}
