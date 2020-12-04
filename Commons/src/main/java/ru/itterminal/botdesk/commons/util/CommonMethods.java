package ru.itterminal.botdesk.commons.util;

import static java.util.Collections.singletonList;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;

@SuppressWarnings({"rawtypes", "unchecked"})
@Slf4j
public class CommonMethods {

    private CommonMethods() {
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

    @SuppressWarnings("ResultOfMethodCallIgnored")
    @SneakyThrows
    public static void chekObjectForNull(Object object, String messageForNull, Class exceptionClass) {
        try {
            object.getClass();
        }
        catch (NullPointerException nullPointerException) {
            log.error(messageForNull);
            throw (Throwable) exceptionClass.getConstructor(String.class).newInstance(messageForNull);
        }
    }

    @SneakyThrows
    public static Map<String, List<ValidationError>> chekObjectForNull(Object object,
                                                                       String keyError,
                                                                       String errorMessage) {
        Map<String, List<ValidationError>> errors = new HashMap<>();
        if (object == null) {
            errors.put(keyError, singletonList(new ValidationError(keyError, errorMessage)));
        }
        return errors;
    }

    public static Map<String, List<ValidationError>>
    chekStringForNullOrEmpty(String object, String keyError,
                             String errorMessageForNull, String errorMessageForEmpty) {
        Map<String, List<ValidationError>> errors = new HashMap<>();
        if (object == null) {
            errors.put(keyError, singletonList(new ValidationError(keyError, errorMessageForNull)));
        }
        if (object != null && object.isEmpty()) {
            errors.put(keyError, singletonList(new ValidationError(keyError, errorMessageForEmpty)));
        }
        return errors;
    }

    @SuppressWarnings("unused")
    public static Map<String, List<ValidationError>>
    chekNumberForNullOrMoreThan(Object number, Long moreThan, String keyError,
                                String errorMessageForNull, String errorMessageIfMoreThan) {
        Map<String, List<ValidationError>> errors = new HashMap<>();
        if (number == null) {
            errors.put(keyError, singletonList(new ValidationError(keyError, errorMessageForNull)));
        }
        if (number instanceof Long && (Long) number > moreThan) {
                errors.put(keyError, singletonList(new ValidationError(keyError, errorMessageIfMoreThan)));
        }
        if (number instanceof Integer && (Integer) number > moreThan) {
            errors.put(keyError, singletonList(new ValidationError(keyError, errorMessageIfMoreThan)));
        }
        return errors;
    }

    @SuppressWarnings("unused")
    public static Map<String, List<ValidationError>>
    chekNumberForNullOrLessThan(Object number, Long lessThan, String keyError,
                                String errorMessageForNull, String errorMessageIfLessThan) {
        Map<String, List<ValidationError>> errors = new HashMap<>();
        if (number == null) {
            errors.put(keyError, singletonList(new ValidationError(keyError, errorMessageForNull)));
        }
        if (number instanceof Long && (Long) number < lessThan) {
            errors.put(keyError, singletonList(new ValidationError(keyError, errorMessageIfLessThan)));
        }
        if (number instanceof Integer && (Integer) number < lessThan) {
            errors.put(keyError, singletonList(new ValidationError(keyError, errorMessageIfLessThan)));
        }
        return errors;
    }

    @SuppressWarnings("unused")
    public static Map<String, List<ValidationError>>
    chekNumberForNullOrZero(Object number, String keyError,
                            String errorMessageForNull, String errorMessageIfZero) {
        Map<String, List<ValidationError>> errors = new HashMap<>();
        if (number == null) {
            errors.put(keyError, singletonList(new ValidationError(keyError, errorMessageForNull)));
        }
        if (number instanceof Long && (Long) number == 0) {
            errors.put(keyError, singletonList(new ValidationError(keyError, errorMessageIfZero)));
        }
        if (number instanceof Integer && (Integer) number == 0) {
            errors.put(keyError, singletonList(new ValidationError(keyError, errorMessageIfZero)));
        }
        return errors;
    }

}
