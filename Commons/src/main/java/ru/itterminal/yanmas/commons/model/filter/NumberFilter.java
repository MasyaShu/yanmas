package ru.itterminal.yanmas.commons.model.filter;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.model.filter.NumberFilter.TypeComparisonForNumberFilter.*;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import ru.itterminal.yanmas.commons.model.validator.enums.ValueOfEnum;
import ru.itterminal.yanmas.commons.util.CommonConstants;

@SuppressWarnings("unused")
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
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
            } catch (Exception exception) {
                throw new IllegalArgumentException(
                        String.format(CommonConstants.INVALID_TYPE_COMPARISON_FOR_VALUE_GIVEN, value), exception);
            }
        }
    }

    @Override
    public boolean IsValid(int max, int min, String regexp) {
        if ((fromString(typeComparison).equals(IS_BETWEEN_EXCLUSION)
                || fromString(typeComparison).equals(IS_BETWEEN_INCLUSIVE)
                || fromString(typeComparison).equals(IS_NOT_BETWEEN_INCLUSIVE)
                || fromString(typeComparison).equals(IS_NOT_BETWEEN_EXCLUSION))) {
            if(valueOne == null || valueTwo == null) {
                throw new IllegalArgumentException(format("ValueOne and ValueTwo must not be null for comparison %s", typeComparison));
            } else if (valueOne.doubleValue() > valueTwo.doubleValue()) {
                throw new IllegalArgumentException("ValueTwo must be greater ValueOne");
            }
        }

        if ((fromString(typeComparison).equals(GREATER_THAN)
                || fromString(typeComparison).equals(GREATER_THAN_OR_EQUAL_TO)
                || fromString(typeComparison).equals(LESS_THAN)
                || fromString(typeComparison).equals(LESS_THAN_OR_EQUAL_TO)
                || fromString(typeComparison).equals(IS_EQUAL_TO)
                || fromString(typeComparison).equals(IS_NOT_EQUAL_TO))
                && valueOne == null) {
                throw new IllegalArgumentException(format("ValueOne must not be null for comparison %s", typeComparison));
        }
        return true;
    }

}
