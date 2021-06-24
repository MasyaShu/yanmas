package ru.itterminal.yanmas.aau.service.validator.property_values.logical_validation;

import ru.itterminal.yanmas.aau.model.PropertyValues;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;

import java.util.List;
import java.util.Map;

import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;

public class CheckIfTheValueMatchesThePropertyTypeValidator implements EntityValidator<PropertyValues> {

    public static final String ERROR_VALUE = "error value";
    public static final String VALUE_IS_NOT_A_NUMBER = "value is not a number";
    public static final String VALUE_IS_NOT_A_BOOLEAN = "value is not a boolean";


    @Override
    public void logicalValidationBeforeUpdate(PropertyValues entity, Map<String, List<ValidationError>> errors) {
        if (entity.getValue() != null) {
            checkIfTheValueMatchesThePropertyType(entity, errors);

        }
    }

    @Override
    public void logicalValidationBeforeCreate(PropertyValues entity, Map<String, List<ValidationError>> errors) {
        if (entity.getValue() != null) {
            checkIfTheValueMatchesThePropertyType(entity, errors);

        }
    }

    private void checkIfTheValueMatchesThePropertyType(PropertyValues entity, Map<String, List<ValidationError>> errors) {
        if (entity.getProperty().getTypeProperty().equals("number")) {
            try {
                Double.parseDouble(entity.getValue());
            } catch (NumberFormatException n) {
                addValidationErrorIntoErrors(
                        ERROR_VALUE,
                        VALUE_IS_NOT_A_NUMBER,
                        errors
                );
            }
        }

        if (entity.getProperty().getTypeProperty().equals("boolean")
                && !entity.getValue().equalsIgnoreCase("true")
                && !entity.getValue().equalsIgnoreCase("false")) {
            addValidationErrorIntoErrors(
                    ERROR_VALUE,
                    VALUE_IS_NOT_A_BOOLEAN,
                    errors
            );
        }
//TODO заменить на enum number and boolean
    }
}
