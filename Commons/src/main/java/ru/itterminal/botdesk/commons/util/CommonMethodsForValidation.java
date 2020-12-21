package ru.itterminal.botdesk.commons.util;

import static java.util.Collections.singletonList;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.FIELDS_ARE_NOT_VALID;
import static ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl.VALIDATION_FAILED;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;

@SuppressWarnings({"rawtypes", "unchecked"})
@Slf4j
public class CommonMethodsForValidation {

    private CommonMethodsForValidation() {
    }

    @SneakyThrows
    public static void chekStringForNullOrEmpty(String string, String messageForNull,
                                                String messageForEmpty, Class exceptionClass, String messageException) {
        String causeException = "";
        if (string == null) {
            causeException = messageForNull;
        }
        if (string != null && string.isEmpty()) {
            causeException = messageForEmpty;
        }
        if (!causeException.isEmpty()) {
            throw (Throwable) exceptionClass.getConstructor(String.class)
                    .newInstance(messageException + causeException);
        }
    }

    @SneakyThrows
    public static void chekObjectForNull(Object object, String messageForNull, Class exceptionClass) {
        if (object == null) {
            log.error(messageForNull);
            throw (Throwable) exceptionClass.getConstructor(String.class).newInstance(messageForNull);
        }
    }

    @SneakyThrows
    public static void chekObjectForNull(Object object,
                                         String keyError,
                                         String errorMessage,
                                         Map<String, List<ValidationError>> errors) {
        if (object == null) {
            addValidationErrorIntoErrors(keyError, errorMessage, errors);
        }
    }

    public static void
    chekStringForNullOrEmpty(String object, String keyError,
                             String errorMessageForNull, String errorMessageForEmpty,
                             Map<String, List<ValidationError>> errors) {

        if (object == null) {
            addValidationErrorIntoErrors(keyError, errorMessageForNull, errors);
        }
        if (object != null && object.isEmpty()) {
            addValidationErrorIntoErrors(keyError, errorMessageForEmpty, errors);
        }
    }

    @SuppressWarnings("unused")
    public static void
    chekNumberForNullOrMoreThan(Object number, Long moreThan, String keyError,
                                String errorMessageForNull, String errorMessageIfMoreThan,
                                Map<String, List<ValidationError>> errors) {
        if (number == null) {
            addValidationErrorIntoErrors(keyError, errorMessageForNull, errors);
        }
        if ((number instanceof Long && (Long) number > moreThan) ||
                (number instanceof Integer && (Integer) number > moreThan)) {
            addValidationErrorIntoErrors(keyError, errorMessageIfMoreThan, errors);
        }
    }

    @SuppressWarnings("unused")
    public static void chekObjectsIsEquals(Object objectOne, Object objectTwo, String keyError,
                                           String errorMessageIfObjectsIsNotEquals,
                                           Map<String, List<ValidationError>> errors) {
        if ((objectOne != null && !objectOne.equals(objectTwo)) ||
                (objectOne == null && objectTwo != null)) {
            addValidationErrorIntoErrors(keyError, errorMessageIfObjectsIsNotEquals, errors);
        }
    }

    @SuppressWarnings("unused")
    public static void
    chekNumberForNullOrZero(Object number, String keyError,
                            String errorMessageForNull, String errorMessageIfZero,
                            Map<String, List<ValidationError>> errors) {
        if (number == null) {
            addValidationErrorIntoErrors(keyError, errorMessageForNull, errors);
        }
        if ((number instanceof Long && (Long) number == 0) ||
                (number instanceof Integer && (Integer) number == 0)) {
            addValidationErrorIntoErrors(keyError, errorMessageIfZero, errors);
        }
    }

    public static Map<String, List<ValidationError>> createMapForLogicalErrors() {
        return new HashMap<>();
    }

    public static void ifErrorsNotEmptyThrowLogicalValidationException(Map<String, List<ValidationError>> errors) {
        if (!errors.isEmpty()) {
            log.error(FIELDS_ARE_NOT_VALID, errors);
            throw new LogicalValidationException(VALIDATION_FAILED, errors);
        }
    }

    public static LogicalValidationException
    createExpectedLogicalValidationException(String keyError, String errorMessage) {
        Map<String, List<ValidationError>> errors = new HashMap<>();
        errors.put(keyError, singletonList(new ValidationError(keyError, errorMessage)));
        return new LogicalValidationException(VALIDATION_FAILED, errors);

    }

    private static void addValidationErrorIntoErrors(String keyError, String errorMessage,
                                                     Map<String, List<ValidationError>> errors) {
        var error = new ValidationError(keyError, errorMessage);
        if (errors.isEmpty() || !errors.containsKey(keyError)) {
            errors.put(keyError, List.of(error));
            return;
        }
        errors.computeIfPresent(keyError, (key, list) -> {
                                    List<ValidationError> validationErrors = new ArrayList<>(list);
                                    validationErrors.add(error);
                                    return validationErrors;
                                }
        );
    }

}
