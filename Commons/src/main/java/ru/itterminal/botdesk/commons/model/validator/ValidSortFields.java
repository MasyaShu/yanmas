package ru.itterminal.botdesk.commons.model.validator;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

@Target({METHOD, FIELD, ANNOTATION_TYPE, CONSTRUCTOR, PARAMETER, TYPE_USE})
@Retention(RUNTIME)
@Documented
@Constraint(validatedBy = SortFieldsValidator.class)
public @interface ValidSortFields {

    String message() default "the sort field is invalid";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};

    String sortFields() default "";

}
