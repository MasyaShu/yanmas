package ru.itterminal.botdesk.commons.model.dto;

import static java.lang.String.format;

import javax.validation.constraints.Size;

import org.springframework.data.domain.Sort;

import lombok.Getter;
import lombok.Setter;
import ru.itterminal.botdesk.commons.model.validator.ValueOfEnum;

/**
 * BaseFilterDto
 *
 */

@Getter
@Setter
public class BaseFilterDto {

    @Size(max = 128)
    private String outId;

    @ValueOfEnum(enumClass = FilterByDeleted.class, message = "must be any of: all, true, false")
    private String deleted = "ALL";

    @ValueOfEnum(enumClass = Sort.Direction.class, message = "must be any of: asc, desc")
    private String direction = "ASC";

    /**
     * Constants for filter entities by deleted field
     * ALL - all entities
     * TRUE - only entities marked as deleted
     * FALSE - only entities unmarked as deleted
     */
    public enum FilterByDeleted {
        TRUE,
        FALSE,
        ALL;

        public static FilterByDeleted fromString(String value) {
            try {
                return valueOf(value.toUpperCase());
            } catch (Exception exception) {
                throw new IllegalArgumentException(
                    format("Invalid value '%s' for deleted given! Has to be either 'all' or 'true' or 'false'",
                        value), exception);
            }
        }
    }
}
