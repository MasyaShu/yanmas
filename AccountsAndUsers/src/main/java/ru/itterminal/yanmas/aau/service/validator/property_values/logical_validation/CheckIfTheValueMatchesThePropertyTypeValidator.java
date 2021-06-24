package ru.itterminal.yanmas.aau.service.validator.property_values.logical_validation;

import ru.itterminal.yanmas.aau.model.PropertyValues;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;

import java.util.List;
import java.util.Map;

public class CheckIfTheValueMatchesThePropertyTypeValidator implements EntityValidator<PropertyValues> {
    @Override
    public void logicalValidationBeforeUpdate(PropertyValues entity, Map<String, List<ValidationError>> errors) {
        checkIfTheValueMatchesThePropertyType(entity, errors);
    }

    @Override
    public void logicalValidationBeforeCreate(PropertyValues entity, Map<String, List<ValidationError>> errors) {
        checkIfTheValueMatchesThePropertyType(entity, errors);
    }

    private void checkIfTheValueMatchesThePropertyType(PropertyValues entity, Map<String, List<ValidationError>> errors) {
//        if(entity.getProperty().getTypeProperty().equals())
    }
}
