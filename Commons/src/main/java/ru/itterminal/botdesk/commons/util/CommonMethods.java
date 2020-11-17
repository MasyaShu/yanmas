package ru.itterminal.botdesk.commons.util;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

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
            throw (Throwable) exceptionClass.getConstructor(String.class).newInstance(messageException + causeException);
        }
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    @SneakyThrows
    public static void chekObjectForNull(Object object, String messageForNull, Class exceptionClass) {
        try {
            object.getClass();
        } catch (NullPointerException nullPointerException) {
            log.error(messageForNull);
            throw (Throwable) exceptionClass.getConstructor(String.class).newInstance(messageForNull);
        }
    }


}
