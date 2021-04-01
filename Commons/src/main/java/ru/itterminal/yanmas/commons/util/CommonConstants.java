package ru.itterminal.yanmas.commons.util;

@SuppressWarnings("unused")
public class CommonConstants {
    public static final String MUST_BE_NULL_FOR_THE_NEW_ENTITY = "field must be null for the new entity";
    public static final String MUST_NOT_BE_NULL = "must not be null";
    public static final String MUST_BE_NULL = "must be null";
    public static final String VALUE_MUST_NOT_BE_NULL = "Value must not be null";
    public static final String MUST_BE_GREATER_THAN_OR_EQUAL_TO_0 = "must be greater than or equal to 0";
    public static final String SIZE_MUST_BE_BETWEEN = "size must be between";
    public static final String REQUEST_NOT_READABLE = "Request not readable";
    public static final String MUST_BE_ANY_OF_ASC_DESC = "must be any of: asc, desc";
    public static final String MESSAGE_NOT_READABLE = "Message Not Readable";
    public static final String DO_NOT_MATCH_THE_AVAILABLE_SORT_VALUES = "do not match the available sort values";
    public static final String INVALID_TYPE_COMPARISON_FOR_VALUE_GIVEN = "Invalid type comparison '%s' for value given!";

    public static final String SPRING_ACTIVE_PROFILE_FOR_INTEGRATION_TESTS = "ITests";
    public static final String SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS = "UTests";
    public static final String SPRING_ACTIVE_PROFILE_FOR_PRODUCTION = "Production";

    private CommonConstants() {
    }
}
