package ru.itterminal.yanmas.aau.util;

public class AAUConstants {
    public static final String EMAIL_PATTERN = "(?:[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\"
            + ".[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+)"
            + "*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-"
            + "\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-zA-Z0-9](?:[a-zA-Z0-9-]*[a-zA-Z0-9])?\\.)+[a-zA-Z0-9]"
            + "(?:[a-zA-Z0-9-]*[a-zA-Z0-9])?|\\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\\.)"
            + "{3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-zA-Z0-9-]*[a-zA-Z0-9]:"
            + "(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b"
            + "\\x0c\\x0e-\\x7f])+)\\])";
    public static final String INVALID_EMAIL = "Invalid email";
    public static final String PASSWORD_PATTERN = "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)(?!.*\\s)[A-Za-z\\d]{6,20}$";
    public static final String INVALID_PASSWORD =
            "Password expression consists only from latin that requires one lower "
                    + "case letter, "
                    + "one upper case letter, one digit, 6-20 length, and no spaces.";

    private AAUConstants() {
    }
}
