package ru.itterminal.botdesk.commons.model.filter;

import static java.lang.String.format;

import java.util.List;
import java.util.UUID;

import lombok.Builder;
import lombok.Data;
import ru.itterminal.botdesk.commons.model.validator.ValueOfEnum;

@SuppressWarnings("unused")
@Data
@Builder
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
                        format("Invalid value '%s' for value given!", value), exception);
            }
        }
    }
}
