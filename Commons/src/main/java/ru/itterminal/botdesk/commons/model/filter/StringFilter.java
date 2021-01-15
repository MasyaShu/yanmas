package ru.itterminal.botdesk.commons.model.filter;

import static java.lang.String.format;

import lombok.Builder;
import lombok.Data;
import ru.itterminal.botdesk.commons.model.validator.ValueOfEnum;

@SuppressWarnings("unused")
@Data
@Builder
public class StringFilter {

    @ValueOfEnum(enumClass = TypeComparisonForStringFilter.class,
            message = "must be any of: is_empty, is_not_empty, text_contains, text_not_contains, text_starts_with, "
                    + "text_ends_with, text_equals")
    private String typeComparison;

    private String text;

    public enum TypeComparisonForStringFilter {
        IS_EMPTY,
        IS_NOT_EMPTY,
        TEXT_CONTAINS,
        TEXT_NOT_CONTAINS,
        TEXT_STARTS_WITH,
        TEXT_ENDS_WITH,
        TEXT_EQUALS;

        public static TypeComparisonForStringFilter fromString(String value) {
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
