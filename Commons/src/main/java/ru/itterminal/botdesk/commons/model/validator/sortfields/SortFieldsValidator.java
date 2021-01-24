package ru.itterminal.botdesk.commons.model.validator.sortfields;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import java.util.*;
import java.util.stream.Collectors;

public class SortFieldsValidator implements ConstraintValidator<ValidateSortFields, List<String>> {

    private String sortFields;
    private String message;


    @Override
    public void initialize(ValidateSortFields constraintAnnotation) {
        sortFields = constraintAnnotation.permittedFieldsForSort();
        message = constraintAnnotation.message();
    }

    @Override
    public boolean isValid(List<String> value, ConstraintValidatorContext context) {

        List<String> sortFieldsList = Arrays.asList(sortFields.toLowerCase().split(", "));

        String notValidField =
                value.stream()
                        .filter(tt -> !sortFieldsList.contains(tt.toLowerCase()))
                        .collect(Collectors.joining(", "));

        if (!notValidField.isEmpty()) {
            message = String.format("the passed values '%s' do not match the available sort values: %s", notValidField, sortFields);
            context.disableDefaultConstraintViolation();
            context.buildConstraintViolationWithTemplate(message)
                    .addConstraintViolation();
            return false;
        }

        return true;
    }
}
