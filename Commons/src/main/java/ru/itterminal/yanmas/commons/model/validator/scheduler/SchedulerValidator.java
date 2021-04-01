package ru.itterminal.yanmas.commons.model.validator.scheduler;

import org.springframework.scheduling.support.CronExpression;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class SchedulerValidator implements ConstraintValidator<ValidateScheduler, String> {

    @Override
    public boolean isValid(String value, ConstraintValidatorContext context) {
        try {
            CronExpression.parse(value);
        } catch (IllegalArgumentException ex) {
            context.disableDefaultConstraintViolation();
            context.buildConstraintViolationWithTemplate(ex.getMessage())
                    .addConstraintViolation();
            return false;
        }
        return true;
    }
}
