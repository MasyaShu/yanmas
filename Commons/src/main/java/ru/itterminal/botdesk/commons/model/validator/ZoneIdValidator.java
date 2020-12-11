package ru.itterminal.botdesk.commons.model.validator;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import java.time.ZoneId;

public class ZoneIdValidator implements ConstraintValidator<ru.itterminal.botdesk.commons.model.validator.ZoneId, String> {

    @Override
    public boolean isValid(String value, ConstraintValidatorContext context) {
        return ZoneId.getAvailableZoneIds().contains(value);
    }
}
