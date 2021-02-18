package ru.itterminal.botdesk.aau.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.filter.BooleanFilter;
import ru.itterminal.botdesk.commons.model.filter.StringFilter;
import ru.itterminal.botdesk.commons.model.validator.filter.ValidateFilter;
import ru.itterminal.botdesk.commons.model.validator.sortfields.ValidateSortFields;

import java.util.List;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class GroupFilterDto extends BaseFilterDto {

    @ValidateFilter(min = 1, max = 128)
    private StringFilter name;

    @ValidateFilter
    private StringFilter comment;

    @ValidateFilter
    private BooleanFilter isDeprecated;

    @ValidateFilter
    private BooleanFilter isInner;

    @ValidateSortFields(permittedFieldsForSort = "deleted, displayName, name, comment, isDeprecated")
    private List<String> sortByFields;

}
