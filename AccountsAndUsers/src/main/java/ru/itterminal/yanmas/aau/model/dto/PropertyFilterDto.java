package ru.itterminal.yanmas.aau.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.commons.model.dto.BaseFilterDto;
import ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter;
import ru.itterminal.yanmas.commons.model.filter.NumberFilter;
import ru.itterminal.yanmas.commons.model.filter.StringFilter;
import ru.itterminal.yanmas.commons.model.validator.filter.ValidateFilter;
import ru.itterminal.yanmas.commons.model.validator.sortfields.ValidateSortFields;

import java.util.List;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class PropertyFilterDto extends BaseFilterDto {

    @ValidateFilter(min = 1, max = 256)
    private StringFilter name;

    @ValidateFilter
    private StringFilter description;

    @ValidateFilter
    private StringFilter typeProperty;

    @ValidateFilter
    private StringFilter entityName;

    @ValidateFilter
    private BaseEntityFilter propertyGroup;

    @ValidateFilter
    private NumberFilter orderView;

    @ValidateSortFields(permittedFieldsForSort = "name, description, orderView")
    private List<String> sortByFields;

}
