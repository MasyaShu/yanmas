package ru.itterminal.botdesk.commons.model.dto;

import static java.lang.String.format;

import java.util.List;

import javax.validation.constraints.Size;

import org.springframework.data.domain.Sort;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.filter.BooleanFilter;
import ru.itterminal.botdesk.commons.model.filter.StringFilter;
import ru.itterminal.botdesk.commons.model.validator.ValueOfEnum;


@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class BaseFilterDtoNew {

    //@Size(max = 128)
    private StringFilter outId;

    private BooleanFilter deleted;

    @ValueOfEnum(enumClass = Sort.Direction.class, message = "must be any of: asc, desc")
    private String sortDirection = "ASC";

    private List<String> sortByFields;

}
