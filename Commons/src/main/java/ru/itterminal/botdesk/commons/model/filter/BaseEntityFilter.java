package ru.itterminal.botdesk.commons.model.filter;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.*;
import static ru.itterminal.botdesk.commons.util.CommonConstants.INVALID_TYPE_COMPARISON_FOR_VALUE_GIVEN;

import java.util.List;
import java.util.UUID;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import ru.itterminal.botdesk.commons.model.validator.enums.ValueOfEnum;

@SuppressWarnings("unused")
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class BaseEntityFilter implements Filter {
    @ValueOfEnum(enumClass = TypeComparisonForBaseEntityFilter.class,
            message = "must be any of: is_empty, is_not_empty, exist_in, not_exist_in")
    private String typeComparison;

    private List<UUID> listOfIdEntities;

    public enum TypeComparisonForBaseEntityFilter {
        IS_EMPTY,
        IS_NOT_EMPTY,
        EXIST_IN,
        NOT_EXIST_IN;

        public static TypeComparisonForBaseEntityFilter fromString(String value) {
            try {
                return valueOf(value.toUpperCase());
            } catch (Exception exception) {
                throw new IllegalArgumentException(
                        format(INVALID_TYPE_COMPARISON_FOR_VALUE_GIVEN, value), exception);
            }
        }
    }

    @Override
    public boolean IsValid(int max, int min, String regexp) {
        if ((fromString(typeComparison).equals(EXIST_IN)
                || fromString(typeComparison).equals(NOT_EXIST_IN))
                && (listOfIdEntities == null || listOfIdEntities.isEmpty())) {
            throw new IllegalArgumentException(format("idEntity must not be null or empty for comparison %s", typeComparison));
        }
        return true;
    }
}
