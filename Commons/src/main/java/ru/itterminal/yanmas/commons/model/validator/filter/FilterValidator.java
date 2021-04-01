package ru.itterminal.yanmas.commons.model.validator.filter;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

import ru.itterminal.yanmas.commons.model.filter.Filter;

public class FilterValidator implements ConstraintValidator<ValidateFilter, Filter> {

    private int max;
    private int min;
    private String regexp;
    private String message;
    private String messageRegexp;

    @Override
    public void initialize(ValidateFilter constraintAnnotation) {
        max = constraintAnnotation.max();
        min = constraintAnnotation.min();
        regexp = constraintAnnotation.regexp();
        message = constraintAnnotation.message();
        messageRegexp = constraintAnnotation.messageRegexp();
    }

    @Override
    public boolean isValid(Filter value, ConstraintValidatorContext context) {
        try {
            return value == null || value.IsValid(max, min, regexp);
        }
        catch (IllegalArgumentException ex) {
            if (ex.getMessage() == null) {
                message = messageRegexp;
            } else {
                message = ex.getMessage();
            }
            context.disableDefaultConstraintViolation();
            context.buildConstraintViolationWithTemplate(message).addConstraintViolation();
            return false;
        }
    }
}
