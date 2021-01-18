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
public class BaseEntityFilter implements Filter {

    @ValueOfEnum(enumClass = TypeComparisonForBaseEntityFilter.class,
            message = "must be any of: is_empty, is_not_empty, exist_in, not_exist_in")
    private String typeComparison;

    private List<UUID> idEntity;

    public enum TypeComparisonForBaseEntityFilter {
        IS_EMPTY,
        IS_NOT_EMPTY,
        EXIST_IN,
        NOT_EXIST_IN;

        public static TypeComparisonForBaseEntityFilter fromString(String value) {
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
