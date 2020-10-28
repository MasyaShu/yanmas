package ru.itterminal.botdesk.aau.util;

public class AAUConstants {
    public final static String emailPattern =  "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\""
            + "(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-"
            + "\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9]"
            + "(?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\\.)"
            + "{3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:"
            + "(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b"
            + "\\x0c\\x0e-\\x7f])+)\\])";
    public final static String MUST_BE_ANY_OF_EN_RU = "must be any of: en, ru";
    public final static String INVALID_EMAIL = "Invalid email";
    public final static String passwordPattern = "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)(?!.*\\s)[A-Za-z\\d]{6,20}$";
    public final static String INVALID_PASSWORD = "Password expression consists only from latin that requires one lower case letter, "
            + "one upper case letter, one digit, 6-20 length, and no spaces.";
    public final static String MUST_BE_ANY_OF_FIRST_NAME_SECOND_NAME = "must be any of: firstName, secondName";
    public final static String MUST_BE_ANY_OF_NAME = "must be any of: name";
}
