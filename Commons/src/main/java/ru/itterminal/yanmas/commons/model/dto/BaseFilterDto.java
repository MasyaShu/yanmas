package ru.itterminal.yanmas.commons.model.dto;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.springframework.data.domain.Sort;
import ru.itterminal.yanmas.commons.model.filter.BooleanFilter;
import ru.itterminal.yanmas.commons.model.filter.StringFilter;
import ru.itterminal.yanmas.commons.model.validator.filter.ValidateFilter;
import ru.itterminal.yanmas.commons.model.validator.enums.ValueOfEnum;


@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class BaseFilterDto {

    @ValidateFilter(max = 128)
    private StringFilter outId;

    @ValidateFilter
    private BooleanFilter deleted;

    @ValueOfEnum(enumClass = Sort.Direction.class, message = "must be any of: asc, desc")
    private String sortDirection = "ASC";

    public List<String> getSortByFields() {
        return null;
    }
}
