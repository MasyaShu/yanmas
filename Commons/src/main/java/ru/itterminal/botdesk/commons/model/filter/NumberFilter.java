package ru.itterminal.botdesk.commons.model.filter;

import static java.lang.String.format;

import lombok.Builder;
import lombok.Data;
import ru.itterminal.botdesk.commons.model.validator.ValueOfEnum;

@SuppressWarnings("unused")
@Data
@Builder
public class NumberFilter implements Filter {

    @ValueOfEnum(enumClass = TypeComparisonForNumberFilter.class,
            message = "must be any of: is_empty, is_not_empty, greater_than, greater_than_or_equal_to, less_than,"
                    + " less_than_or_equal_to, is_between_inclusive, is_between_exclusion, is_not_between_inclusive,"
                    + " is_not_between_exclusion, is_equal_to, is_not_equal_to")
    private String typeComparison;

    private Number valueOne;
    private Number valueTwo;

    public enum TypeComparisonForNumberFilter {
        IS_EMPTY,
        IS_NOT_EMPTY,
        GREATER_THAN,
        GREATER_THAN_OR_EQUAL_TO,
        LESS_THAN,
        LESS_THAN_OR_EQUAL_TO,
        IS_BETWEEN_INCLUSIVE,
        IS_BETWEEN_EXCLUSION,
        IS_NOT_BETWEEN_INCLUSIVE,
        IS_NOT_BETWEEN_EXCLUSION,
        IS_EQUAL_TO,
        IS_NOT_EQUAL_TO;

        public static TypeComparisonForNumberFilter fromString(String value) {
            try {
                return valueOf(value.toUpperCase());
            }
            catch (Exception exception) {
                throw new IllegalArgumentException(
                        format("Invalid value '%s' for value given!", value), exception);
            }
        }
    }
}
