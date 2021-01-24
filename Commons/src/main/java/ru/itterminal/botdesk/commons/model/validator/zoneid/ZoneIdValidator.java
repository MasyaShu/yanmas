package ru.itterminal.botdesk.commons.model.validator.zoneid;

import java.time.ZoneId;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class ZoneIdValidator implements ConstraintValidator<ValidateZoneId, String> {

    @Override
    public boolean isValid(String value, ConstraintValidatorContext context) {
        return ZoneId.getAvailableZoneIds().contains(value);
    }
}
