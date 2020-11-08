package ru.itterminal.botdesk.commons.util;

@SuppressWarnings({"rawtypes", "unchecked"})
public class CommonMethods {
    public static void chekStringForNullOrEmpty(String string, String messageForNull,
            String messageForEmpty, Class exceptionClass, String messageException) throws Throwable {
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

    private CommonMethods() {
    }
}
