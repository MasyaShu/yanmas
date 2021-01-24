package ru.itterminal.botdesk.commons.model.filter;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.model.filter.StringFilter.TypeComparisonForStringFilter.*;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import ru.itterminal.botdesk.commons.model.validator.enums.ValueOfEnum;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

@SuppressWarnings("unused")
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class StringFilter implements Filter {

    @ValueOfEnum(enumClass = TypeComparisonForStringFilter.class,
            message = "must be any of: is_empty, is_not_empty, text_contains, text_not_contains, text_starts_with, "
                    + "text_ends_with, text_equals")
    private String typeComparison;

    private String value;

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
            } catch (Exception exception) {
                throw new IllegalArgumentException(
                        format("Invalid value '%s' for value given!", value), exception);
            }
        }

    }

    @Override
    public boolean IsValid(int max, int min, String regexp) {
        if ((fromString(typeComparison).equals(TEXT_CONTAINS)
                || fromString(typeComparison).equals(TEXT_NOT_CONTAINS)
                || fromString(typeComparison).equals(TEXT_STARTS_WITH)
                || fromString(typeComparison).equals(TEXT_ENDS_WITH)
                || fromString(typeComparison).equals(TEXT_EQUALS))
                && (value == null || value.isEmpty())) {
            throw new IllegalArgumentException(format("Value must not be null or empty for comparison %s", typeComparison));
        } else {
            if (value.length() > max || value.length() < min) {
                throw new IllegalArgumentException(format("size must be between %s and %s", min, max));
            }
            if (!regexp.isEmpty()) {
                Pattern pattern = Pattern.compile(regexp);
                Matcher matcher = pattern.matcher(value);
                if (!matcher.find()) {
                    throw new IllegalArgumentException();
                }
            }
        }
        return true;
    }
}
