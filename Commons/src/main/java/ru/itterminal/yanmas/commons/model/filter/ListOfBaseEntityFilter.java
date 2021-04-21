package ru.itterminal.yanmas.commons.model.filter;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.*;

import java.util.List;
import java.util.UUID;

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
public  class ListOfBaseEntityFilter implements Filter{

    @ValueOfEnum(enumClass = TypeComparisonForListOfBaseEntityFilter.class,
            message = "must be any of: is_empty, is_not_empty, is_equal_to, is_not_equal_to, "
                    + "not_contains_any_of_list, contains_any_of_list, contains_all_of_list, not_contains_all_of_list")
    private String typeComparison;

    private List<UUID> listOfIdEntities;

    public enum TypeComparisonForListOfBaseEntityFilter {
        IS_EMPTY,
        IS_NOT_EMPTY,
        IS_EQUAL_TO,
        IS_NOT_EQUAL_TO,
        NOT_CONTAINS_ANY_IN_LIST,
        CONTAINS_ANY_IN_LIST,
        CONTAINS_ALL_OF_LIST,
        NOT_CONTAINS_ALL_OF_LIST;

        public static TypeComparisonForListOfBaseEntityFilter fromString(String value) {
            try {
                return valueOf(value.toUpperCase());
            }
            catch (Exception exception) {
                throw new IllegalArgumentException(
                        String.format(CommonConstants.INVALID_TYPE_COMPARISON_FOR_VALUE_GIVEN, value), exception);
            }
        }
    }

    @Override
    public boolean IsValid(int max, int min, String regexp) {
        if ((fromString(typeComparison).equals(IS_EQUAL_TO)
                || fromString(typeComparison).equals(IS_NOT_EQUAL_TO)
                || fromString(typeComparison).equals(NOT_CONTAINS_ANY_IN_LIST)
                || fromString(typeComparison).equals(CONTAINS_ANY_IN_LIST)
                || fromString(typeComparison).equals(CONTAINS_ALL_OF_LIST)
                || fromString(typeComparison).equals(NOT_CONTAINS_ALL_OF_LIST))
                && (listOfIdEntities == null || listOfIdEntities.isEmpty())) {
            throw new IllegalArgumentException(format("listOfIdEntities must not be null or empty for comparison %s", typeComparison));
        }
        return true;
    }
}
